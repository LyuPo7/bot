{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes, FlexibleInstances #-}

module Bot.Mode.Mode where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad.Catch (MonadThrow)

import qualified Bot.Logger.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.DB.DBQ as BotDBQ
import qualified Bot.Request.Request as BotReq
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Api as BotApi
import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Objects.MessageType as BotMessageType
import qualified Bot.Objects.Mode as BotMode
import qualified Bot.Api.Tele.Mode.Mode as TeleMode
import qualified Bot.Api.Vk.Mode.Mode as VkMode
import qualified Bot.Util as BotUtil

class (MonadThrow m, Monad m) => Worker m api where
  setupBot :: api ->
              BotReq.Handle m ->
              m ()
  getFirstUpdate :: api ->
                    BotReq.Handle m ->
                    m BotUpdate.Update
  getLastUpdate :: api ->
                   BotReq.Handle m ->
                   BotUpdate.Update ->
                   m (Maybe (BotSynonyms.UpdateId, BotMessage.Message))
  getNextUpdate :: api ->
                   BotReq.Handle m ->
                   BotUpdate.Update ->
                   m BotUpdate.Update
  getPrevUpdate :: api ->
                   BotReq.Handle m ->
                   BotUpdate.Update ->
                   m BotUpdate.Update
  getChatId :: api ->
               BotReq.Handle m -> 
               BotMessage.Message ->
               m BotSynonyms.ChatId
  getMessageText :: api ->
                    BotReq.Handle m ->
                    BotMessage.Message ->
                    m (Maybe Text)
  getMessageType :: api ->
                    BotReq.Handle m ->
                    BotMessage.Message ->
                    m BotMessageType.MessageType

instance (MonadThrow m, Monad m) => Worker m BotApi.Api where
  setupBot BotApi.Tele h = TeleMode.setupBot h
  setupBot BotApi.Vk _ = return ()

  getFirstUpdate BotApi.Tele h = TeleMode.getFirstUpdate h
  getFirstUpdate BotApi.Vk h = VkMode.getFirstUpdate h

  getLastUpdate BotApi.Tele h update = TeleMode.getLastUpdate h update
  getLastUpdate BotApi.Vk h update = VkMode.getLastUpdate h update

  getNextUpdate BotApi.Tele h update = TeleMode.getNextUpdate h update
  getNextUpdate BotApi.Vk h update = VkMode.getNextUpdate h update

  getPrevUpdate BotApi.Tele h update = TeleMode.getPrevUpdate h update
  getPrevUpdate BotApi.Vk h update = VkMode.getPrevUpdate h update

  getChatId BotApi.Tele h message = TeleMode.getChatId h message
  getChatId BotApi.Vk h message = VkMode.getChatId h message

  getMessageText BotApi.Tele h message = TeleMode.getMessageText h message
  getMessageText BotApi.Vk h message = VkMode.getMessageText h message

  getMessageType BotApi.Tele h message = TeleMode.getMessageType h message
  getMessageType BotApi.Vk h message = VkMode.getMessageType h message

startMode :: (MonadThrow m, Monad m) =>
              BotReq.Handle m ->
              m ()
startMode handle = do
  let logH = BotReq.hLogger handle
      config = BotReq.cReq handle
      api = Settings.botApi config
  Logger.logInfo logH $ "Bot api: "
    <> BotUtil.convertValue api
  setupBot api handle
  firstUpdate <- getFirstUpdate api handle
  _ <- checkMode handle firstUpdate
  return ()

checkMode :: (MonadThrow m, Monad m) =>
              BotReq.Handle m ->
              BotUpdate.Update ->
              m (Maybe BotSynonyms.UpdateId)
checkMode handle prevUpdate = do
  let logH = BotReq.hLogger handle
      dbH = BotReq.hDb handle
      config = BotReq.cReq handle
      api = Settings.botApi config
  nextUpdate <- getLastUpdate api handle prevUpdate
  case nextUpdate of
    Nothing -> do
      Logger.logWarning logH "Where are no more updates for now!"
      Logger.logInfo logH "Waiting updates!"
      newUpdate <- getPrevUpdate api handle prevUpdate
      _ <- checkMode handle newUpdate
      return Nothing
    Just (updateId, userMessage) -> do
      Logger.logInfo logH $ "Checking update with id: "
        <> BotUtil.convertValue updateId
      chatId <- getChatId api handle userMessage
      mode <- BotDBQ.getMode dbH chatId
      case mode of
        BotMode.ReplyMode -> do
          _ <- replyMode handle userMessage
          Logger.logDebug logH "Bot in reply mode."
        BotMode.AnswerMode -> do
          _ <- answerMode handle userMessage
          Logger.logDebug logH "Bot in answer mode."
        BotMode.UnknownMode -> do
          Logger.logError logH $ "Unknown Bot mode: "
            <> BotUtil.convertValue mode
      BotDBQ.putUpdate dbH updateId
      newUpdate <- getNextUpdate api handle prevUpdate
      _ <- checkMode handle newUpdate
      return $ Just updateId

replyMode :: (MonadThrow m, Monad m) =>
              BotReq.Handle m ->
              BotMessage.Message ->
              m BotMode.Mode
replyMode handle userMessage = do
  let logH = BotReq.hLogger handle
      dbH = BotReq.hDb handle
      config = BotReq.cReq handle
      api = Settings.botApi config
  chatId <- getChatId api handle userMessage
  messageType <- getMessageType api handle userMessage
  case messageType of
    BotMessageType.TextMessage _ -> do
      repNum <- BotDBQ.getRepliesNumber dbH chatId
      BotReq.sendNEchoMessage handle userMessage repNum
      Logger.logInfo logH "It's ordinary message from User."
      return BotMode.ReplyMode
    BotMessageType.HelpMessage -> do
      Logger.logInfo logH "User's /help message"
      _ <- BotReq.sendHelpMessage handle userMessage
      return BotMode.ReplyMode
    BotMessageType.RepeatMessage -> do
      _ <- BotReq.sendKeyboard handle userMessage
      Logger.logInfo logH "Info: User's /repeat message"
      BotDBQ.setMode dbH chatId BotMode.AnswerMode
      return BotMode.AnswerMode
    BotMessageType.StartMessage -> do
      Logger.logInfo logH "User's /start message"
      BotReq.sendStartMessage handle userMessage
      return BotMode.ReplyMode
    BotMessageType.UnsupportedMessage -> do
      Logger.logError logH "Received unsupported bot-command!"
      return BotMode.ReplyMode

answerMode :: (MonadThrow m, Monad m) =>
               BotReq.Handle m ->
               BotMessage.Message ->
               m (Maybe BotSynonyms.RepNum)
answerMode handle userMessage = do
  let logH = BotReq.hLogger handle
      dbH = BotReq.hDb handle
      config = BotReq.cReq handle
      api = Settings.botApi config
  chatId <- getChatId api handle userMessage
  messageTextM <- getMessageText api handle userMessage
  let messageText = T.unpack $ fromMaybe "" messageTextM
  BotDBQ.setMode dbH chatId BotMode.ReplyMode
  case (readMaybe messageText :: Maybe BotSynonyms.RepNum) of
    Just repNum -> do
      Logger.logInfo logH $ "Info: Received user's answer in chat with id: "
        <> BotUtil.convertValue chatId
      BotDBQ.setRepliesNumber dbH chatId repNum
      return $ Just repNum
    Nothing -> do
      Logger.logError logH "Couldn't parse User's answer!"
      return Nothing
module Bot.Mode.Mode where

import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad.Catch (MonadThrow)

import qualified Bot.Logger.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Request.Request as BotReq
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Objects.MessageType as BotMessageType
import qualified Bot.Objects.Mode as BotMode
import qualified Bot.Mode.Api as BotApiMode
import qualified Bot.Util as BotUtil

startMode :: (MonadThrow m, Monad m) =>
              BotReq.Handle m ->
              m ()
startMode handle = do
  let logH = BotReq.hLogger handle
      config = BotReq.cReq handle
      api = Settings.botApi config
  Logger.logInfo logH $ "Bot api: "
    <> BotUtil.convertValue api
  BotApiMode.setupBot api handle
  firstUpdate <- BotApiMode.getFirstUpdate api handle
  _ <- checkMode handle firstUpdate
  return ()

checkMode :: (MonadThrow m, Monad m) =>
              BotReq.Handle m ->
              BotUpdate.Update ->
              m (Maybe BotSynonyms.UpdateId)
checkMode handle prevUpdate = do
  let logH = BotReq.hLogger handle
      config = BotReq.cReq handle
      api = Settings.botApi config
  nextUpdate <- getLastUpdate handle prevUpdate
  case nextUpdate of
    Nothing -> do
      Logger.logWarning logH "Where are no more updates for now!"
      Logger.logInfo logH "Waiting updates!"
      newUpdate <- BotApiMode.getPrevUpdate api handle prevUpdate
      _ <- checkMode handle newUpdate
      return Nothing
    Just (updateId, userMessage) -> do
      Logger.logInfo logH $ "Checking update with id: "
        <> BotUtil.convertValue updateId
      chatId <- BotApiMode.getChatId api handle userMessage
      mode <- BotReq.getMode handle chatId
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
      BotReq.putUpdate handle updateId
      newUpdate <- BotApiMode.getNextUpdate api handle prevUpdate
      _ <- checkMode handle newUpdate
      return $ Just updateId

replyMode :: (MonadThrow m, Monad m) =>
              BotReq.Handle m ->
              BotMessage.Message ->
              m BotMode.Mode
replyMode handle userMessage = do
  let logH = BotReq.hLogger handle
      config = BotReq.cReq handle
      api = Settings.botApi config
  chatId <- BotApiMode.getChatId api handle userMessage
  messageType <- BotApiMode.getMessageType api handle userMessage
  case messageType of
    BotMessageType.TextMessage _ -> do
      repNum <- BotReq.getRepliesNumber handle chatId
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
      BotReq.setMode handle chatId BotMode.AnswerMode
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
      config = BotReq.cReq handle
      api = Settings.botApi config
  chatId <- BotApiMode.getChatId api handle userMessage
  messageTextM <- BotApiMode.getMessageText api handle userMessage
  let messageText = T.unpack $ fromMaybe "" messageTextM
  BotReq.setMode handle chatId BotMode.ReplyMode
  case (readMaybe messageText :: Maybe BotSynonyms.RepNum) of
    Just repNum -> do
      Logger.logInfo logH $ "Info: Received user's answer in chat with id: "
        <> BotUtil.convertValue chatId
      BotReq.setRepliesNumber handle chatId repNum
      return $ Just repNum
    Nothing -> do
      Logger.logError logH "Couldn't parse User's answer!"
      return Nothing

getLastUpdate :: (Monad m, MonadThrow m) =>
                  BotReq.Handle m ->
                  BotUpdate.Update ->
                  m (Maybe (BotSynonyms.UpdateId, BotMessage.Message))
getLastUpdate handle update = do
  let logH = BotReq.hLogger handle
      config = BotReq.cReq handle
      api = Settings.botApi config
  defaultUpdateId <- BotApiMode.getDefaultUpdateId api handle update
  lastDbUpdateId <- BotReq.getLastSucUpdate handle
  let nextDbUpdateId = fmap (+1) lastDbUpdateId
      nextUpdateId = fromMaybe defaultUpdateId nextDbUpdateId
  nextUpdate <- BotApiMode.changeUpdateId api handle update nextUpdateId
  response <- BotReq.getUpdate handle nextUpdate
  newUpdate <- BotApiMode.extractUpdate api handle response
  case newUpdate of
    [] -> return Nothing
    [x] -> do
      messageM <- BotApiMode.extractMessage api handle x
      case messageM of
        Nothing -> return Nothing
        Just message -> return $ Just (nextUpdateId, message)
    _ -> do
      Logger.logError logH "Expected one update, but have many."
      return Nothing
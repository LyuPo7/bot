module Bot.Api.Tele.Mode.Mode where

import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Convertible.Base (convert)

import qualified Bot.Exception as E
import qualified Bot.Logger.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.DB.DBQ as BotDBQ
import qualified Bot.Request.Request as BotReq
import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Mode.Mode as BotMode
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Objects.MessageType as BotMessageType
import qualified Bot.Api.Tele.Parser.Parser as TeleParser
import qualified Bot.Api.Tele.Objects.Chat as TeleChat
import qualified Bot.Api.Tele.Objects.Update as TeleUpdate
import qualified Bot.Api.Tele.Objects.UpdateData as TeleUpData
import qualified Bot.Api.Tele.Objects.Message as TeleMessage
import qualified Bot.Api.Tele.Objects.MessageEntity as TeleMessageEntity

withHandleIO :: Logger.Handle IO -> Settings.Config -> BotDBQ.Handle IO ->
                BotReq.Handle IO -> BotParser.Handle IO ->
                (BotMode.Handle IO -> IO a) -> IO a
withHandleIO logger config dbH reqH parserH f = do
  let handle = BotMode.Handle {
    BotMode.hLogger = logger,
    BotMode.cRun = config,
    BotMode.hDb = dbH,
    BotMode.hReq = reqH,
    BotMode.hParser = parserH,

    BotMode.setupBot = setupBot reqH,
    BotMode.getFirstUpdate = getFirstUpdate reqH,
    BotMode.getLastUpdate = getLastUpdate reqH,
    BotMode.getNextUpdate = getNextUpdate reqH,
    BotMode.getPrevUpdate = getPrevUpdate reqH,
    BotMode.getChatId = getChatId reqH,
    BotMode.getMessageText = getMessageText reqH,
    BotMode.getMessageType = getMessageType reqH
  }
  f handle

setupBot :: (Monad m, MonadThrow m) => BotReq.Handle m -> m ()
setupBot handle = do
  BotReq.sendCommands handle

getLastUpdate :: (Monad m, MonadThrow m) => BotReq.Handle m ->
                  BotUpdate.Update ->
                  m (Maybe (BotSynonyms.UpdateId, BotMessage.Message))
getLastUpdate handle _ = do
  let logH = BotReq.hLogger handle
      dbH = BotReq.hDb handle
      parserH = BotReq.hParser handle
  processedUpdId <- BotDBQ.getLastSucUpdate dbH
  let newUpdId = fmap (+ 1) processedUpdId
      nextUpdateId = fromMaybe 0 newUpdId
      nextUpdate = BotUpdate.TeleUpdate nextUpdateId
  responseUp <- BotReq.getUpdate handle nextUpdate
  updateData <- TeleParser.parseUpdateData parserH responseUp
  let update = TeleUpData.result updateData
  case update of
    [] -> return Nothing
    [x] -> do
      let updateId = TeleUpdate.id x
          checkMessage = TeleUpdate.message x
      case checkMessage of
        Nothing -> do 
          Logger.logWarning logH "Where are no new messages!"
          return Nothing
        Just userMessage -> do
          let botMessage = BotMessage.TeleMessage userMessage
          return $ Just (updateId, botMessage)
    _ -> do
      Logger.logError logH "Expected one update, but have many."
      return Nothing

getFirstUpdate :: Monad m => BotReq.Handle m -> m BotUpdate.Update
getFirstUpdate _ = return $ BotUpdate.TeleUpdate 0

getNextUpdate :: (MonadThrow m, Monad m) => BotReq.Handle m ->
                  BotUpdate.Update -> m BotUpdate.Update
getNextUpdate _ (BotUpdate.TeleUpdate updateId) = do
  return $ BotUpdate.TeleUpdate (updateId + 1)
getNextUpdate _ botUpdate = do
  throwM $ E.ApiObjectError $ show botUpdate

getPrevUpdate :: (MonadThrow m, Monad m) => BotReq.Handle m ->
                  BotUpdate.Update -> m BotUpdate.Update
getPrevUpdate _ (BotUpdate.TeleUpdate updateId) = do
  return $ BotUpdate.TeleUpdate (updateId - 1)
getPrevUpdate _ botUpdate = do
  throwM $ E.ApiObjectError $ show botUpdate

getChatId :: (MonadThrow m, Monad m) => BotReq.Handle m -> BotMessage.Message -> m BotSynonyms.ChatId
getChatId _ (BotMessage.TeleMessage userMessage) = do
  return $ TeleChat.id $ TeleMessage.chat userMessage
getChatId _ botMessage = do
  throwM $ E.ApiObjectError $ show botMessage

getMessageText :: (MonadThrow m, Monad m) => BotReq.Handle m -> BotMessage.Message -> m (Maybe Text)
getMessageText _ (BotMessage.TeleMessage userMessage) = do
  return $ TeleMessage.text userMessage
getMessageText _ botMessage = do
  throwM $ E.ApiObjectError $ show botMessage

getMessageType :: (MonadThrow m, Monad m) => BotReq.Handle m -> BotMessage.Message ->
                  m BotMessageType.MessageType
getMessageType _ (BotMessage.TeleMessage userMessage) = do
  let messageTextM = TeleMessage.text userMessage
  case messageTextM of
    Nothing -> return $ BotMessageType.TextMessage ""
    Just messageText -> do
      let entitiesM = filter 
           ((== "bot_command") . TeleMessageEntity.entity_type) 
           <$> TeleMessage.entities userMessage
      case entitiesM of
        Nothing -> return $ BotMessageType.TextMessage messageText
        Just _ -> return $ convert messageText
getMessageType _ botMessage = do
  throwM $ E.ApiObjectError $ show botMessage
  
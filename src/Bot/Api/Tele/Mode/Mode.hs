module Bot.Api.Tele.Mode.Mode where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Convertible.Base (convert)

import qualified Bot.Exception as E
import qualified Bot.Logger.Logger as Logger
import qualified Bot.Request.Request as BotReq
import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Objects.MessageType as BotMessageType
import qualified Bot.Api.Tele.Objects.Chat as TeleChat
import qualified Bot.Api.Tele.Objects.Update as TeleUpdate
import qualified Bot.Api.Tele.Objects.UpdateData as TeleUpData
import qualified Bot.Api.Tele.Objects.Message as TeleMessage
import qualified Bot.Api.Tele.Objects.MessageEntity as TeleMessageEntity

setupBot :: (Monad m, MonadThrow m) =>
             BotReq.Handle m ->
             m ()
setupBot handle = do
  BotReq.sendCommands handle

extractUpdate :: (Monad m, MonadThrow m) =>
                  BotReq.Handle m ->
                  L8.ByteString ->
                  m [BotUpdate.Update]
extractUpdate handle response = do
  let dbH = BotReq.hDb handle
  updateDataE <- BotParser.parseData dbH response
  case updateDataE of
    Left msg -> throwM $ E.ParseRequestError $ T.unpack msg
    Right updateData -> do
      let update = TeleUpData.result updateData
      return $ fmap BotUpdate.TeleFullUpdate update

extractMessage :: (Monad m, MonadThrow m) =>
                  BotReq.Handle m ->
                  BotUpdate.Update ->
                  m (Maybe BotMessage.Message)
extractMessage handle (BotUpdate.TeleFullUpdate update) = do
  let logH = BotReq.hLogger handle
      checkMessage = TeleUpdate.message update
  case checkMessage of
    Nothing -> do 
      Logger.logWarning logH "Where are no new messages!"
      return Nothing
    Just userMessage -> do
      let botMessage = BotMessage.TeleMessage userMessage
      return $ Just botMessage
extractMessage _ botUpdate = do
  throwM $ E.ApiObjectError $ show botUpdate

getFirstUpdate :: Monad m =>
                  BotReq.Handle m ->
                  m BotUpdate.Update
getFirstUpdate _ = return $ BotUpdate.TeleUpdate 0

getNextUpdate :: (MonadThrow m, Monad m) =>
                  BotReq.Handle m ->
                  BotUpdate.Update ->
                  m BotUpdate.Update
getNextUpdate _ (BotUpdate.TeleUpdate updateId) = do
  return $ BotUpdate.TeleUpdate (updateId + 1)
getNextUpdate _ botUpdate = do
  throwM $ E.ApiObjectError $ show botUpdate

getPrevUpdate :: (MonadThrow m, Monad m) =>
                  BotReq.Handle m ->
                  BotUpdate.Update ->
                  m BotUpdate.Update
getPrevUpdate _ (BotUpdate.TeleUpdate updateId) = do
  return $ BotUpdate.TeleUpdate (updateId - 1)
getPrevUpdate _ botUpdate = do
  throwM $ E.ApiObjectError $ show botUpdate

getChatId :: (MonadThrow m, Monad m) =>
              BotReq.Handle m ->
              BotMessage.Message ->
              m BotSynonyms.ChatId
getChatId _ (BotMessage.TeleMessage userMessage) = do
  return $ TeleChat.id $ TeleMessage.chat userMessage
getChatId _ botMessage = do
  throwM $ E.ApiObjectError $ show botMessage

getMessageText :: (MonadThrow m, Monad m) =>
                   BotReq.Handle m ->
                   BotMessage.Message ->
                   m (Maybe Text)
getMessageText _ (BotMessage.TeleMessage userMessage) = do
  return $ TeleMessage.text userMessage
getMessageText _ botMessage = do
  throwM $ E.ApiObjectError $ show botMessage

getMessageType :: (MonadThrow m, Monad m) =>
                   BotReq.Handle m ->
                   BotMessage.Message ->
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

changeUpdateId :: (MonadThrow m, Monad m) =>
                   BotReq.Handle m ->
                   BotUpdate.Update ->
                   BotSynonyms.UpdateId ->
                   m BotUpdate.Update
changeUpdateId _ (BotUpdate.TeleUpdate _) updateId = do
  return $ BotUpdate.TeleUpdate updateId
changeUpdateId _ botUpdate _ = do
  throwM $ E.ApiObjectError $ show botUpdate
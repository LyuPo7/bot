module Bot.Api.Vk.Mode.Mode where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Data.Text (Text)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Convertible.Base (convert)

import qualified Bot.Exception as E
import qualified Bot.Logger.Logger as Logger
import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Request.Request as BotReq
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Objects.MessageType as BotMessageType
import qualified Bot.Api.Vk.Objects.Message as VkMessage
import qualified Bot.Api.Vk.Objects.Server as VkServer 
import qualified Bot.Api.Vk.Objects.UpdateData as VkUpData
import qualified Bot.Api.Vk.Objects.Update as VkUpdate
import qualified Bot.Api.Vk.Objects.PollResponse as VkPollResp
import qualified Bot.Util as BotUtil

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
      let updates = VkUpData.updates updateData
      return $ fmap BotUpdate.VkFullUpdate updates

extractMessage :: (Monad m, MonadThrow m) =>
                  BotReq.Handle m ->
                  BotUpdate.Update ->
                  m (Maybe BotMessage.Message)
extractMessage handle (BotUpdate.VkFullUpdate update) = do
  let logH = BotReq.hLogger handle
      updateType = VkUpdate.update_type update
      userMessage = VkUpdate.object update
  case updateType of
    "message_new" -> do
      let botMessage = BotMessage.VkMessage userMessage
      return $ Just botMessage
    _ -> do
      Logger.logError logH "Unsupported type of update."
      return Nothing
extractMessage _ botUpdate = do
  throwM $ E.ApiObjectError $ show botUpdate

getFirstUpdate :: (MonadThrow m, Monad m) =>
                   BotReq.Handle m ->
                   m BotUpdate.Update
getFirstUpdate handle = do
  let dbH = BotReq.hDb handle
  serverUp <- BotReq.getServer handle
  serverParamsE <- runEitherT $ do
    params <- newEitherT $ BotParser.parseData dbH serverUp
    let tsText = VkServer.text_ts $ VkPollResp.response params
    tsInt <- newEitherT $ BotUtil.readValue tsText
    return VkServer.Server {
      VkServer.key = VkServer.text_key $ VkPollResp.response params,
      VkServer.server = VkServer.text_server $ VkPollResp.response params,
      VkServer.ts = tsInt
    }
  case serverParamsE of
    Left msg2 -> throwM $ E.ParseRequestError $ T.unpack msg2
    Right serverParams -> return $ BotUpdate.VkUpdate serverParams

getNextUpdate :: (MonadThrow m, Monad m) =>
                  BotReq.Handle m ->
                  BotUpdate.Update ->
                  m BotUpdate.Update
getNextUpdate _ (BotUpdate.VkUpdate update) = do
  let updateId = VkServer.ts update
      newUpdate = update {VkServer.ts = updateId + 1}
  return $ BotUpdate.VkUpdate newUpdate
getNextUpdate _ botUpdate = do
  throwM $ E.ApiObjectError $ show botUpdate
  
getPrevUpdate :: (MonadThrow m, Monad m) =>
                  BotReq.Handle m ->
                  BotUpdate.Update ->
                  m BotUpdate.Update
getPrevUpdate _ (BotUpdate.VkUpdate update) = do
  let updateId = VkServer.ts update
      newUpdate = update {VkServer.ts = updateId - 1}
  return $ BotUpdate.VkUpdate newUpdate
getPrevUpdate _ botUpdate = do
  throwM $ E.ApiObjectError $ show botUpdate

getChatId :: (MonadThrow m, Monad m) =>
              BotReq.Handle m ->
              BotMessage.Message ->
              m BotSynonyms.ChatId
getChatId _ (BotMessage.VkMessage userMessage) = do
  return $ VkMessage.user_id userMessage
getChatId _ botMessage = do
  throwM $ E.ApiObjectError $ show botMessage

getMessageText :: (MonadThrow m, Monad m) =>
                   BotReq.Handle m ->
                   BotMessage.Message ->
                   m (Maybe Text)
getMessageText _ (BotMessage.VkMessage userMessage) = do
  return $ Just $ VkMessage.body userMessage
getMessageText _ botMessage = do
  throwM $ E.ApiObjectError $ show botMessage

getMessageType :: (MonadThrow m, Monad m) =>
                   BotReq.Handle m ->
                   BotMessage.Message ->
                   m BotMessageType.MessageType
getMessageType _ (BotMessage.VkMessage userMessage) = do
  let messageText = VkMessage.body userMessage
  case convert messageText of
    BotMessageType.StartMessage -> return BotMessageType.UnsupportedMessage
    message -> return message
getMessageType _ botMessage = do
  throwM $ E.ApiObjectError $ show botMessage

getDefaultUpdateId :: (MonadThrow m, Monad m) =>
                       BotReq.Handle m ->
                       BotUpdate.Update ->
                       m BotSynonyms.UpdateId
getDefaultUpdateId _ (BotUpdate.VkUpdate serverParams) = do
  return $ convert $ VkServer.ts serverParams
getDefaultUpdateId _ botUpdate = do
  throwM $ E.ApiObjectError $ show botUpdate

changeUpdateId :: (MonadThrow m, Monad m) =>
                   BotReq.Handle m ->
                   BotUpdate.Update ->
                   BotSynonyms.UpdateId ->
                   m BotUpdate.Update
changeUpdateId _ (BotUpdate.VkUpdate update) newUpdateId = do
  let newUpdate = update {VkServer.ts = convert newUpdateId}
  return $ BotUpdate.VkUpdate newUpdate
changeUpdateId _ botUpdate _ = do
  throwM $ E.ApiObjectError $ show botUpdate
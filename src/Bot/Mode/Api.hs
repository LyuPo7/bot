module Bot.Mode.Api where

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Convertible.Base (convert)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Bot.Api.Tele.Objects.Chat as TeleChat
import qualified Bot.Api.Tele.Objects.Message as TeleMessage
import qualified Bot.Api.Tele.Objects.MessageEntity as TeleMessageEntity
import qualified Bot.Api.Tele.Objects.Update as TeleUpdate
import qualified Bot.Api.Tele.Objects.UpdateData as TeleUpData
import qualified Bot.Api.Vk.Objects.Message as VkMessage
import qualified Bot.Api.Vk.Objects.PollResponse as VkPollResp
import qualified Bot.Api.Vk.Objects.Server as VkServer
import qualified Bot.Api.Vk.Objects.Update as VkUpdate
import qualified Bot.Api.Vk.Objects.UpdateData as VkUpData
import qualified Bot.Exception as E
import qualified Bot.Logger.Logger as Logger
import qualified Bot.Objects.Api as BotApi
import qualified Bot.Objects.FullUpdate as BotFullUpdate
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Objects.MessageType as BotMessageType
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Objects.UpdateData as BotUpData
import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Request.Request as BotReq
import qualified Bot.Util as BotUtil

setupBot ::
  (MonadThrow m, Monad m) =>
  BotApi.Api ->
  BotReq.Handle m ->
  m ()
setupBot BotApi.Tele handle = BotReq.sendCommands handle
setupBot BotApi.Vk _ = return ()

getFirstUpdate ::
  (MonadThrow m, Monad m) =>
  BotApi.Api ->
  BotReq.Handle m ->
  m BotUpdate.Update
getFirstUpdate BotApi.Tele handle = do
  let dbH = BotReq.hDb handle
  response <- BotReq.getFirstUpdate handle
  updateDataM <- BotParser.parseData dbH response
  case updateDataM of
    Left msg2 -> throwM $ E.ParseRequestError $ T.unpack msg2
    Right updateData -> do
      case TeleUpData.result updateData of
        (update : _) -> do
          let updateId = TeleUpdate.id update
          return $ BotUpdate.TeleUpdate updateId
        _ -> throwM E.FirstUpdateError
getFirstUpdate BotApi.Vk handle = do
  let dbH = BotReq.hDb handle
  serverUp <- BotReq.getServer handle
  serverParamsE <- runEitherT $ do
    params <- newEitherT $ BotParser.parseData dbH serverUp
    let tsText = VkServer.text_ts $ VkPollResp.response params
    tsInt <- newEitherT $ BotUtil.readValue tsText
    return
      VkServer.Server
        { VkServer.key = VkServer.text_key $ VkPollResp.response params,
          VkServer.server = VkServer.text_server $ VkPollResp.response params,
          VkServer.ts = tsInt
        }
  case serverParamsE of
    Left msg2 -> throwM $ E.ParseRequestError $ T.unpack msg2
    Right serverParams -> return $ BotUpdate.VkUpdate serverParams

getNextUpdate ::
  (MonadThrow m, Monad m) =>
  BotReq.Handle m ->
  BotUpdate.Update ->
  m BotUpdate.Update
getNextUpdate _ (BotUpdate.TeleUpdate updateId) = do
  return $ BotUpdate.TeleUpdate (updateId + 1)
getNextUpdate _ (BotUpdate.VkUpdate update) = do
  let updateId = VkServer.ts update
      newUpdate = update {VkServer.ts = updateId + 1}
  return $ BotUpdate.VkUpdate newUpdate

getPrevUpdate ::
  (MonadThrow m, Monad m) =>
  BotReq.Handle m ->
  BotUpdate.Update ->
  m BotUpdate.Update
getPrevUpdate _ (BotUpdate.TeleUpdate updateId) = do
  return $ BotUpdate.TeleUpdate (updateId - 1)
getPrevUpdate _ (BotUpdate.VkUpdate update) = do
  let updateId = VkServer.ts update
      newUpdate = update {VkServer.ts = updateId - 1}
  return $ BotUpdate.VkUpdate newUpdate

getChatId ::
  (MonadThrow m, Monad m) =>
  BotReq.Handle m ->
  BotMessage.Message ->
  m BotSynonyms.ChatId
getChatId _ (BotMessage.TeleMessage userMessage) = do
  return $ TeleChat.id $ TeleMessage.chat userMessage
getChatId _ (BotMessage.VkMessage userMessage) = do
  return $ VkMessage.user_id userMessage

getMessageText ::
  (MonadThrow m, Monad m) =>
  BotReq.Handle m ->
  BotMessage.Message ->
  m (Maybe Text)
getMessageText _ (BotMessage.TeleMessage userMessage) = do
  return $ TeleMessage.text userMessage
getMessageText _ (BotMessage.VkMessage userMessage) = do
  return $ Just $ VkMessage.body userMessage

getMessageType ::
  (MonadThrow m, Monad m) =>
  BotReq.Handle m ->
  BotMessage.Message ->
  m BotMessageType.MessageType
getMessageType _ (BotMessage.TeleMessage userMessage) = do
  let messageTextM = TeleMessage.text userMessage
  case messageTextM of
    Nothing -> return $ BotMessageType.TextMessage ""
    Just messageText -> do
      let entitiesM =
            filter
              ((== "bot_command") . TeleMessageEntity.entity_type)
              <$> TeleMessage.entities userMessage
      case entitiesM of
        Nothing -> return $ BotMessageType.TextMessage messageText
        Just _ -> return $ convert messageText
getMessageType _ (BotMessage.VkMessage userMessage) = do
  let messageText = VkMessage.body userMessage
  case convert messageText of
    BotMessageType.StartMessage -> return BotMessageType.UnsupportedMessage
    message -> return message

getDefaultUpdateId ::
  (MonadThrow m, Monad m) =>
  BotReq.Handle m ->
  BotUpdate.Update ->
  m BotSynonyms.UpdateId
getDefaultUpdateId _ (BotUpdate.VkUpdate serverParams) = do
  return $ convert $ VkServer.ts serverParams
getDefaultUpdateId _ (BotUpdate.TeleUpdate updateId) = do
  return updateId

changeUpdateId ::
  (MonadThrow m, Monad m) =>
  BotReq.Handle m ->
  BotUpdate.Update ->
  BotSynonyms.UpdateId ->
  m BotUpdate.Update
changeUpdateId _ (BotUpdate.TeleUpdate _) updateId = do
  return $ BotUpdate.TeleUpdate updateId
changeUpdateId _ (BotUpdate.VkUpdate update) newUpdateId = do
  let newUpdate = update {VkServer.ts = convert newUpdateId}
  return $ BotUpdate.VkUpdate newUpdate

extractUpdate ::
  (MonadThrow m, Monad m) =>
  BotReq.Handle m ->
  BotUpData.UpdateData ->
  m [BotFullUpdate.FullUpdate]
extractUpdate _ (BotUpData.TeleUpdateData updateData) = do
  let updates = TeleUpData.result updateData
  return $ fmap BotFullUpdate.TeleUpdate updates
extractUpdate _ (BotUpData.VkUpdateData updateData) = do
  let updates = VkUpData.updates updateData
  return $ fmap BotFullUpdate.VkUpdate updates

extractMessage ::
  (MonadThrow m, Monad m) =>
  BotReq.Handle m ->
  BotFullUpdate.FullUpdate ->
  m (Maybe BotMessage.Message)
extractMessage handle botUpdate = do
  let logH = BotReq.hLogger handle
  case botUpdate of
    BotFullUpdate.TeleUpdate update -> do
      let checkMessage = TeleUpdate.message update
      case checkMessage of
        Nothing -> do
          Logger.logWarning logH "Where are no new messages!"
          return Nothing
        Just userMessage -> do
          let botMessage = BotMessage.TeleMessage userMessage
          return $ Just botMessage
    BotFullUpdate.VkUpdate update -> do
      let updateType = VkUpdate.update_type update
          userMessage = VkUpdate.object update
      case updateType of
        "message_new" -> do
          let botMessage = BotMessage.VkMessage userMessage
          return $ Just botMessage
        _ -> do
          Logger.logError logH "Unsupported type of update."
          return Nothing

getUpdateData ::
  (Monad m, MonadThrow m) =>
  BotApi.Api ->
  BotReq.Handle m ->
  L8.ByteString ->
  m (Either Text BotUpData.UpdateData)
getUpdateData api handle response
  | api == BotApi.Tele = do
    updateDataE <- BotParser.parseData dbH response
    return (BotUpData.TeleUpdateData <$> updateDataE)
  | otherwise = do
    updateDataE <- BotParser.parseData dbH response
    return (BotUpData.VkUpdateData <$> updateDataE)
 where
  dbH = BotReq.hDb handle

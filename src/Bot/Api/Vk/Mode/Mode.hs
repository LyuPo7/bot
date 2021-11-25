module Bot.Api.Vk.Mode.Mode where

import qualified Data.Text as T
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Convertible.Base (convert)

import qualified Bot.Exception as E
import qualified Bot.Logger.Logger as Logger
import qualified Bot.DB.DBQ as BotDBQ
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

getLastUpdate :: (MonadThrow m, Monad m) =>
                  BotReq.Handle m ->
                  BotUpdate.Update ->
                  m (Maybe (BotSynonyms.UpdateId, BotMessage.Message))
getLastUpdate handle (BotUpdate.VkUpdate serverParams) = do
  let logH = BotReq.hLogger handle
      dbH = BotReq.hDb handle
      tsCurrent = VkServer.ts serverParams
  processedUpdId <- BotDBQ.getLastSucUpdate dbH
  let newUpdId = fmap (+1) processedUpdId
      updateId = fromMaybe (BotSynonyms.UpdateId tsCurrent) newUpdId
  let nextUpdate = BotUpdate.VkUpdate serverParams
  responseUp <- BotReq.getUpdate handle nextUpdate
  updateDataE <- BotParser.parseData dbH responseUp
  case updateDataE of
    Left msg -> throwM $ E.ParseRequestError $ T.unpack msg
    Right updateData -> do
      let update = VkUpData.updates updateData
      case update of
        [] -> return Nothing
        (x:_) -> do
          let updateType = VkUpdate.update_type x
              userMessage = VkUpdate.object x
          case updateType of
            "message_new" -> do
              let botMessage = BotMessage.VkMessage userMessage
              return $ Just (updateId, botMessage)
            _ -> do
              Logger.logError logH "Unsupported type of update."
              return Nothing
getLastUpdate _ botUpdate = do
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
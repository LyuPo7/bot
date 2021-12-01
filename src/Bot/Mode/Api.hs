module Bot.Mode.Api where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import Control.Monad.Catch (MonadThrow)

import qualified Bot.Request.Request as BotReq
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Api as BotApi
import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Objects.MessageType as BotMessageType
import qualified Bot.Api.Tele.Mode.Mode as TeleMode
import qualified Bot.Api.Vk.Mode.Mode as VkMode

setupBot :: (MonadThrow m, Monad m) =>
             BotApi.Api ->
             BotReq.Handle m ->
             m ()
setupBot BotApi.Tele h = TeleMode.setupBot h
setupBot BotApi.Vk _ = return ()

getFirstUpdate :: (MonadThrow m, Monad m) =>
                   BotApi.Api ->
                   BotReq.Handle m ->
                   m BotUpdate.Update
getFirstUpdate BotApi.Tele h = TeleMode.getFirstUpdate h
getFirstUpdate BotApi.Vk h = VkMode.getFirstUpdate h

getNextUpdate :: (MonadThrow m, Monad m) =>
                  BotApi.Api ->
                  BotReq.Handle m ->
                  BotUpdate.Update ->
                  m BotUpdate.Update
getNextUpdate BotApi.Tele h update = TeleMode.getNextUpdate h update
getNextUpdate BotApi.Vk h update = VkMode.getNextUpdate h update

getPrevUpdate :: (MonadThrow m, Monad m) =>
                  BotApi.Api ->
                  BotReq.Handle m ->
                  BotUpdate.Update ->
                  m BotUpdate.Update
getPrevUpdate BotApi.Tele h update = TeleMode.getPrevUpdate h update
getPrevUpdate BotApi.Vk h update = VkMode.getPrevUpdate h update

getChatId :: (MonadThrow m, Monad m) =>
              BotApi.Api ->
              BotReq.Handle m -> 
              BotMessage.Message ->
              m BotSynonyms.ChatId
getChatId BotApi.Tele h message = TeleMode.getChatId h message
getChatId BotApi.Vk h message = VkMode.getChatId h message

getMessageText :: (MonadThrow m, Monad m) =>
                   BotApi.Api ->
                   BotReq.Handle m ->
                   BotMessage.Message ->
                   m (Maybe Text)
getMessageText BotApi.Tele h message = TeleMode.getMessageText h message
getMessageText BotApi.Vk h message = VkMode.getMessageText h message

getMessageType :: (MonadThrow m, Monad m) =>
                   BotApi.Api ->
                   BotReq.Handle m ->
                   BotMessage.Message ->
                   m BotMessageType.MessageType
getMessageType BotApi.Tele h message = TeleMode.getMessageType h message
getMessageType BotApi.Vk h message = VkMode.getMessageType h message

getDefaultUpdateId :: (MonadThrow m, Monad m) =>
                       BotApi.Api ->
                       BotReq.Handle m ->
                       BotUpdate.Update ->
                       m BotSynonyms.UpdateId
getDefaultUpdateId BotApi.Tele _ _ = return 0
getDefaultUpdateId BotApi.Vk h update = VkMode.getDefaultUpdateId h update

changeUpdateId :: (MonadThrow m, Monad m) =>
                   BotApi.Api ->
                   BotReq.Handle m ->
                   BotUpdate.Update ->
                   BotSynonyms.UpdateId ->
                   m BotUpdate.Update
changeUpdateId BotApi.Tele h update updateId =
  TeleMode.changeUpdateId h update updateId
changeUpdateId BotApi.Vk h update updateId =
  VkMode.changeUpdateId h update updateId

extractUpdate :: (MonadThrow m, Monad m) =>
                  BotApi.Api ->
                  BotReq.Handle m ->
                  L8.ByteString ->
                  m [BotUpdate.Update]
extractUpdate BotApi.Tele h response = TeleMode.extractUpdate h response
extractUpdate BotApi.Vk h response = VkMode.extractUpdate h response

extractMessage :: (MonadThrow m, Monad m) =>
                   BotApi.Api ->
                   BotReq.Handle m ->
                   BotUpdate.Update ->
                   m (Maybe BotMessage.Message)
extractMessage BotApi.Tele h update = TeleMode.extractMessage h update
extractMessage BotApi.Vk h update = VkMode.extractMessage h update
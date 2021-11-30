module Bot.Api.Tele.Request.Requests where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Convertible.Base (convert)

import qualified Bot.Exception as E
import qualified Bot.DB.DB as BotDB
import qualified Bot.Settings as Settings
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.RequestPair as BotReqPair
import qualified Bot.Objects.Button as BotButton
import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Api.Tele.Objects.Method as TeleMethod
import qualified Bot.Api.Tele.Objects.RequestOptions as TeleReqOptions
import qualified Bot.Api.Tele.Objects.Message as TeleMessage
import qualified Bot.Api.Tele.Objects.Chat as TeleChat
import qualified Bot.Api.Tele.Objects.Keyboard as TeleKeyboard
import qualified Bot.Api.Tele.Objects.GetUpdates as TeleGetUpdates
import qualified Bot.Api.Tele.Objects.SendMessage as TeleSendMessage
import qualified Bot.Api.Tele.Objects.KeyboardMessage as TeleKeyboardMessage
import qualified Bot.Api.Tele.Objects.CopyMessage as TeleCopyMessage
import qualified Bot.Api.Tele.Objects.SetCommands as TeleSetCommands

setGetUpdate :: (MonadThrow m, Monad m) =>
                 BotDB.Handle m ->
                 BotUpdate.Update ->
                 m BotReqPair.ReqPair
setGetUpdate _ (BotUpdate.TeleUpdate updateId) = do
  let reqOptions = TeleReqOptions.GetUpdates $
                   TeleGetUpdates.createGetUpdates updateId
      apiMethod = TeleMethod.getUpdates
  return $ BotReqPair.TeleReqPair (apiMethod, reqOptions)
setGetUpdate _ botUpdate = do
  throwM $ E.ApiObjectError $ show botUpdate

setEchoMessage :: (MonadThrow m, Monad m) =>
                   BotDB.Handle m ->
                   BotMessage.Message ->
                   m BotReqPair.ReqPair
setEchoMessage _ (BotMessage.TeleMessage message) = do
  let chatId = TeleChat.id $ TeleMessage.chat message
      messageId = TeleMessage.message_id message
      reqOptions = TeleReqOptions.CopyMessage $
                   TeleCopyMessage.createEchoMessage chatId messageId
      apiMethod = TeleMethod.copyMessage
  return $ BotReqPair.TeleReqPair (apiMethod, reqOptions)
setEchoMessage _ botMessage = do
  throwM $ E.ApiObjectError $ show botMessage

setTextMessage :: (MonadThrow m, Monad m) =>
                  BotDB.Handle m ->
                  BotMessage.Message ->
                  Text ->
                  m (Maybe BotReqPair.ReqPair)
setTextMessage _ (BotMessage.TeleMessage message) text = do
  let chatId = TeleChat.id $ TeleMessage.chat message
      reqOptions = TeleReqOptions.SendMessage $
                   TeleSendMessage.createTextMessage chatId text
      apiMethod = TeleMethod.sendMessage
  return $ Just $ BotReqPair.TeleReqPair (apiMethod, reqOptions)
setTextMessage _ botMessage _ = do
  throwM $ E.ApiObjectError $ show botMessage

setKeyboardMessage :: (MonadThrow m, Monad m) =>
                       BotDB.Handle m ->
                       BotMessage.Message ->
                      [BotButton.Button] ->
                       Text ->
                       m BotReqPair.ReqPair
setKeyboardMessage _ (BotMessage.TeleMessage message) buttons question = do
  let chatId = TeleChat.id $ TeleMessage.chat message
  if length buttons == 5
    then do
      let teleButtons = fmap BotButton.createTeleButton buttons
          keyboard = TeleKeyboard.createKeyboard [teleButtons]
          apiMethod = TeleMethod.sendMessage
          reqOptions = TeleReqOptions.SendKeyboard $ 
            TeleKeyboardMessage.createKeyboardMessage chatId question keyboard
      return $ BotReqPair.TeleReqPair (apiMethod, reqOptions)
    else throwM $ E.ButtonNumberError $ length buttons
setKeyboardMessage _ botMessage _ _ = do
  throwM $ E.ApiObjectError $ show botMessage

setCommands :: Monad m =>
               BotDB.Handle m ->
               m (Maybe BotReqPair.ReqPair)
setCommands _ = do
  let reqOptions = TeleReqOptions.SetCommands TeleSetCommands.createCommands
      apiMethod = TeleMethod.setCommands
  return $ Just $ BotReqPair.TeleReqPair (apiMethod, reqOptions)

createRequest :: (Monad m, MonadThrow m) =>
                  BotDB.Handle m ->
                  BotReqPair.ReqPair ->
                  m (Text, B.ByteString)
createRequest handle (BotReqPair.TeleReqPair (apiMethod, options)) = do
  let config = BotDB.cDb handle
      token = Settings.botToken config
      hostApi = BotSynonyms.getHost Settings.apiTele
      methodApi = TeleMethod.getMethod apiMethod
      api = T.concat [hostApi, convert token, methodApi]
      apiOptions = TeleReqOptions.encodeRequestOptions options
  return (api, apiOptions)
createRequest _ _ = do
  throwM E.ApiMethodError
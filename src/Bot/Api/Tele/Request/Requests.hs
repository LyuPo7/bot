module Bot.Api.Tele.Request.Requests where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import  qualified Network.HTTP.Client as HTTPClient
import Data.Text (Text)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Convertible.Base (convert)

import qualified Bot.Exception as E
import qualified Bot.Logger.Logger as Logger
import qualified Bot.DB.DBQ as BotDBQ
import qualified Bot.Settings as Settings
import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Request.Request as BotReq
import qualified Bot.Objects.Method as BotMethod
import qualified Bot.Objects.RequestOptions as BotReqOptions
import qualified Bot.Objects.Button as BotButton
import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Api.Tele.Objects.Method as TeleMethod
import qualified Bot.Api.Tele.Objects.RequestOptions as TeleReqOptions
import qualified Bot.Api.Tele.Objects.Message as TeleMessage
import qualified Bot.Api.Tele.Objects.Chat as TeleChat
import qualified Bot.Api.Tele.Objects.Keyboard as TeleKeyboard
import qualified Bot.Api.Tele.Objects.Button as TeleButton
import qualified Bot.Api.Tele.Objects.GetUpdates as TeleGetUpdates
import qualified Bot.Api.Tele.Objects.SendMessage as TeleSendMessage
import qualified Bot.Api.Tele.Objects.KeyboardMessage as TeleKeyboardMessage
import qualified Bot.Api.Tele.Objects.CopyMessage as TeleCopyMessage
import qualified Bot.Api.Tele.Objects.SetCommands as TeleSetCommands

withHandleIO :: Logger.Handle IO -> BotDBQ.Handle IO -> BotParser.Handle IO ->
                Settings.Config -> (BotReq.Handle IO -> IO a) -> IO a
withHandleIO logger dbH parserH config f = do
  let handle = BotReq.Handle {
    BotReq.hLogger = logger,
    BotReq.hDb = dbH,
    BotReq.hParser = parserH,
    BotReq.cReq = config,

    BotReq.createRequest = createRequest parserH,
    BotReq.setUploadedServer = \_ -> return Nothing,
    BotReq.setUploadedDoc = \_ -> return Nothing,
    BotReq.setGetServer = return Nothing,
    BotReq.setGetUpdate = setGetUpdate parserH,
    BotReq.setEchoMessage = setEchoMessage parserH,
    BotReq.setHelpMessage = setTextMessage parserH,
    BotReq.setStartMessage = setTextMessage parserH,
    BotReq.setKeyboardMessage = setKeyboardMessage parserH,
    BotReq.setCommands = setCommands parserH,

    BotReq.downloadDoc = \_ _ -> return Nothing,
    BotReq.extractDoc = \_ -> return Nothing,
    BotReq.changeMessage = \message _-> return message,
    BotReq.changeDoc = \doc _ -> return doc,

    BotReq.newManager = HTTPClient.newManager,
    BotReq.httpLbs = HTTPClient.httpLbs
  }
  f handle

setGetUpdate :: (MonadThrow m, Monad m) => BotParser.Handle m -> BotUpdate.Update ->
                m (BotMethod.Method, BotReqOptions.RequestOptions)
setGetUpdate _ (BotUpdate.TeleUpdate updateId) = do
  let reqOptions = BotReqOptions.TeleReqOptions $
                   TeleReqOptions.GetUpdates $
                   TeleGetUpdates.createGetUpdates updateId
      apiMethod = BotMethod.TeleMethod TeleMethod.getUpdates
  return (apiMethod, reqOptions)
setGetUpdate _ botUpdate = do
  throwM $ E.ApiObjectError $ show botUpdate

setEchoMessage :: (MonadThrow m, Monad m) => BotParser.Handle m -> BotMessage.Message ->
                  m (BotMethod.Method, BotReqOptions.RequestOptions)
setEchoMessage _ (BotMessage.TeleMessage message) = do
  let chatId = TeleChat.id $ TeleMessage.chat message
      messageId = TeleMessage.message_id message
      reqOptions = BotReqOptions.TeleReqOptions
        (TeleReqOptions.CopyMessage $
         TeleCopyMessage.createEchoMessage chatId messageId
        )
      apiMethod = BotMethod.TeleMethod TeleMethod.copyMessage
  return (apiMethod, reqOptions)
setEchoMessage _ botMessage = do
  throwM $ E.ApiObjectError $ show botMessage

setTextMessage :: (MonadThrow m, Monad m) =>
                  BotParser.Handle m ->
                  BotMessage.Message ->
                  Text ->
                  m (Maybe (BotMethod.Method, BotReqOptions.RequestOptions))
setTextMessage _ (BotMessage.TeleMessage message) text = do
  let chatId = TeleChat.id $ TeleMessage.chat message
      reqOptions = BotReqOptions.TeleReqOptions 
        (TeleReqOptions.SendMessage $
         TeleSendMessage.createTextMessage chatId text
        )
      apiMethod = BotMethod.TeleMethod TeleMethod.sendMessage
  return $ Just (apiMethod, reqOptions)
setTextMessage _ botMessage _ = do
  throwM $ E.ApiObjectError $ show botMessage

setKeyboardMessage :: (MonadThrow m, Monad m) => BotParser.Handle m ->
                      BotMessage.Message ->
                     [BotButton.Button] ->
                      Text ->
                      m (BotMethod.Method, BotReqOptions.RequestOptions)
setKeyboardMessage _ (BotMessage.TeleMessage message) buttons question = do
  let chatId = TeleChat.id $ TeleMessage.chat message
      [b1, b2, b3, b4, b5] = buttons
      b1Tele = TeleButton.createButton
        (BotButton.text b1) (BotButton.description b1)
      b2Tele = TeleButton.createButton
        (BotButton.text b2) (BotButton.description b2)
      b3Tele = TeleButton.createButton
        (BotButton.text b3) (BotButton.description b3)
      b4Tele = TeleButton.createButton
        (BotButton.text b4) (BotButton.description b4)
      b5Tele = TeleButton.createButton
        (BotButton.text b5) (BotButton.description b5)
      keyboard = TeleKeyboard.createKeyboard
        [[b1Tele, b2Tele, b3Tele, b4Tele, b5Tele]]
      apiMethod = BotMethod.TeleMethod TeleMethod.sendMessage
      reqOptions = BotReqOptions.TeleReqOptions
        (TeleReqOptions.SendKeyboard $ 
         TeleKeyboardMessage.createKeyboardMessage chatId question keyboard
        )
  return (apiMethod, reqOptions)
setKeyboardMessage _ botMessage _ _ = do
  throwM $ E.ApiObjectError $ show botMessage

setCommands :: Monad m => BotParser.Handle m ->
               m (Maybe (BotMethod.Method, BotReqOptions.RequestOptions))
setCommands _ = do
  let commands = TeleReqOptions.SetCommands TeleSetCommands.createCommands
      apiMethod = BotMethod.TeleMethod TeleMethod.setCommands
      reqOptions = BotReqOptions.TeleReqOptions commands
  return $ Just (apiMethod, reqOptions)

createRequest :: (Monad m, MonadThrow m) =>
                  BotParser.Handle m ->
                  BotMethod.Method ->
                  BotReqOptions.RequestOptions ->
                  m (Text, B.ByteString)
createRequest handle (BotMethod.TeleMethod apiMethod)
                     (BotReqOptions.TeleReqOptions options) = do
  let config = BotParser.cParser handle
      token = Settings.botToken config
      hostApi = Settings.getHost Settings.apiTele
      methodApi = TeleMethod.getMethod apiMethod
      api = T.concat [hostApi, convert token, methodApi]
      apiOptions = TeleReqOptions.encodeRequestOptions options
  return (api, apiOptions)
createRequest _ _ _ = do
  throwM E.ApiMethodError
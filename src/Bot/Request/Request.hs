{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes, FlexibleInstances #-}

module Bot.Request.Request where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTPClient
import Data.Text (Text)
import Control.Monad.Catch (MonadThrow, throwM)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.Convertible.Base (convert)

import qualified Bot.Exception as E
import qualified Bot.Logger.Logger as Logger
import qualified Bot.DB.DBQ as BotDBQ
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Api as BotApi
import qualified Bot.Objects.RequestPair as BotReqPair
import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Objects.Button as BotButton
import qualified Bot.Objects.Document as BotDoc
import qualified Bot.Settings as Settings
import qualified Bot.Util as BotUtil
import qualified Bot.Api.Vk.Request.Requests as VkReq
import qualified Bot.Api.Tele.Request.Requests as TeleReq

data Handle m = Handle {
  hLogger :: Logger.Handle m,
  hDb :: BotDBQ.Handle m,
  cReq :: Settings.Config,

  newManager :: HTTPClient.ManagerSettings ->
                m HTTPClient.Manager,
  httpLbs :: HTTPClient.Request ->
             HTTPClient.Manager ->
             m (HTTPClient.Response B.ByteString)
}

class (MonadThrow m, Monad m) => Request m api where
  createRequest :: api ->
                   BotDBQ.Handle m ->
                   BotReqPair.ReqPair ->
                   m (Text, B.ByteString)
  setGetUpdate :: api ->
                  BotDBQ.Handle m ->
                  BotUpdate.Update ->
                  m BotReqPair.ReqPair
  setEchoMessage :: api ->
                    BotDBQ.Handle m ->
                    BotMessage.Message ->
                    m BotReqPair.ReqPair
  setHelpMessage :: api ->
                    BotDBQ.Handle m ->
                    BotMessage.Message ->
                    Text ->
                    m (Maybe BotReqPair.ReqPair)
  setStartMessage :: api ->
                     BotDBQ.Handle m ->
                     BotMessage.Message ->
                     Text ->
                     m (Maybe BotReqPair.ReqPair)
  setKeyboardMessage :: api ->
                        BotDBQ.Handle m ->
                        BotMessage.Message ->
                       [BotButton.Button] ->
                        Text ->
                        m BotReqPair.ReqPair
  setCommands :: api ->
                 BotDBQ.Handle m ->
                 m (Maybe BotReqPair.ReqPair)
  changeMessage :: api ->
                   BotDBQ.Handle m ->
                   BotMessage.Message ->
                  [BotDoc.Document] ->
                   m BotMessage.Message
  extractDoc :: api ->
                BotDBQ.Handle m ->
                BotMessage.Message ->
                m (Maybe [BotDoc.Document])
  setUploadedServer :: api ->
                       BotDBQ.Handle m ->
                       BotDoc.Document ->
                       m (Maybe BotReqPair.ReqPair)
  setUploadedDoc :: api ->
                    BotDBQ.Handle m ->
                    Text ->
                    m (Maybe BotReqPair.ReqPair)
  setGetServer :: api ->
                  BotDBQ.Handle m ->
                  m (Maybe BotReqPair.ReqPair)
  downloadDoc :: api ->
                 BotDBQ.Handle m ->
                 BotDoc.Document ->
                 B.ByteString ->
                 m (Maybe Text)
  changeDoc :: api ->
               BotDBQ.Handle m ->
               BotDoc.Document ->
               B.ByteString ->
               m BotDoc.Document

instance (MonadThrow m, Monad m) => Request m BotApi.Api where
  createRequest BotApi.Tele h pair = TeleReq.createRequest h pair
  createRequest BotApi.Vk h pair = VkReq.createRequest h pair

  setUploadedServer BotApi.Tele _ _ = return Nothing
  setUploadedServer BotApi.Vk h doc = VkReq.setUploadedServer h doc

  setUploadedDoc BotApi.Tele _ _ = return Nothing
  setUploadedDoc BotApi.Vk h text = VkReq.setUploadedDoc h text

  setGetServer BotApi.Tele _ = return Nothing
  setGetServer BotApi.Vk h = VkReq.setGetServer h

  setGetUpdate BotApi.Tele h update = TeleReq.setGetUpdate h update
  setGetUpdate BotApi.Vk h update = VkReq.setGetUpdate h update

  setEchoMessage BotApi.Tele h message = TeleReq.setEchoMessage h message
  setEchoMessage BotApi.Vk h message = VkReq.setEchoMessage h message

  setHelpMessage BotApi.Tele h message text =
    TeleReq.setTextMessage h message text
  setHelpMessage BotApi.Vk h message text = VkReq.setHelpMessage h message text

  setStartMessage BotApi.Tele h message text =
    TeleReq.setTextMessage h message text
  setStartMessage BotApi.Vk _ _ _ = return Nothing

  setKeyboardMessage BotApi.Tele h message buttons text =
    TeleReq.setKeyboardMessage h message buttons text
  setKeyboardMessage BotApi.Vk h message buttons text =
    VkReq.setKeyboardMessage h message buttons text

  setCommands BotApi.Tele h = TeleReq.setCommands h
  setCommands BotApi.Vk _ = return Nothing

  downloadDoc BotApi.Tele _ _ _ = return Nothing
  downloadDoc BotApi.Vk h doc bstr = VkReq.downloadDoc h doc bstr

  extractDoc BotApi.Tele _ _ = return Nothing
  extractDoc BotApi.Vk h message = VkReq.extractDoc h message

  changeMessage BotApi.Tele _ message _ = return message
  changeMessage BotApi.Vk h message docs = VkReq.changeMessage h message docs

  changeDoc BotApi.Vk h doc bstr = VkReq.changeDoc h doc bstr
  changeDoc BotApi.Tele _ doc _ = return doc
  

getServer :: (MonadThrow m, Monad m) =>
              Handle m ->
              m B.ByteString
getServer handle = do
  let logH = hLogger handle
      dbH = hDb handle
      config = cReq handle
      api = Settings.botApi config
  reqPairM <- setGetServer api dbH
  case reqPairM of
    Nothing -> do
      Logger.logError logH "No needs in server parameters for this API"
      throwM E.GetServerError
    Just reqPair -> do
      Logger.logInfo logH "Get server parameters for requests."
      makeRequest handle reqPair

getUploadedServer :: (MonadThrow m, Monad m) =>
                      Handle m ->
                      BotDoc.Document ->
                      m B.ByteString
getUploadedServer handle doc = do
  let logH = hLogger handle
      dbH = hDb handle
      config = cReq handle
      api = Settings.botApi config
  reqPairM <- setUploadedServer api dbH doc
  case reqPairM of
    Nothing -> do
      Logger.logWarning logH "No needs uploaded server for this API"
      throwM E.UploadedServerError
    Just reqPair -> do
      Logger.logInfo logH "Get Uploaded server parameters for upload file."
      makeRequest handle reqPair

getUpdate :: (Monad m, MonadThrow m) =>
              Handle m ->
              BotUpdate.Update ->
              m B.ByteString
getUpdate handle update = do
  let dbH = hDb handle
      config = cReq handle
      api = Settings.botApi config
  reqPair <- setGetUpdate api dbH update
  makeRequest handle reqPair

sendEchoMessage :: (MonadThrow m, Monad m) =>
                    Handle m ->
                    BotMessage.Message ->
                    m BotMessage.Message
sendEchoMessage handle message = do
  let logH = hLogger handle
      dbH = hDb handle
      config = cReq handle
      api = Settings.botApi config
  newMessage <- updateMessage handle message
  reqPair <- setEchoMessage api dbH newMessage
  _ <- makeRequest handle reqPair
  Logger.logInfo logH "Echo-Message was forwarded."
  return newMessage

sendNEchoMessage :: (MonadThrow m, Monad m) =>
                     Handle m ->
                     BotMessage.Message ->
                     BotSynonyms.RepNum ->
                     m ()
sendNEchoMessage handle _ 0 = do
  let logH = hLogger handle
  Logger.logInfo logH "Echo-Messages were sent."
sendNEchoMessage handle message n = do
  _ <- sendEchoMessage handle message
  sendNEchoMessage handle message (n - 1)

sendHelpMessage :: (MonadThrow m, Monad m) =>
                    Handle m ->
                    BotMessage.Message ->
                    m BotReqPair.ReqPair
sendHelpMessage handle message = do
  let logH = hLogger handle
      dbH = hDb handle
      config = cReq handle
      api = Settings.botApi config
      helpText = Settings.botDescription config
  reqPairM <- setHelpMessage api dbH message (convert helpText)
  case reqPairM of
    Nothing -> do
      Logger.logWarning logH "No exist '/help' command for this API"
      throwM E.HelpMessageError
    Just reqPair -> do
      _ <- makeRequest handle reqPair
      Logger.logInfo logH "Help-Message was sent."
      return reqPair

sendStartMessage :: (MonadThrow m, Monad m) =>
                     Handle m ->
                     BotMessage.Message ->
                     m ()
sendStartMessage handle message = do
  let logH = hLogger handle
      dbH = hDb handle
      config = cReq handle
      api = Settings.botApi config
      startText = "You are welcome!"
  reqPairM <- setStartMessage api dbH message startText
  case reqPairM of
    Nothing -> do
      Logger.logWarning logH "No exist '/start' command for this API"
      throwM E.StartMessageError
    Just reqPair -> do
      _ <- makeRequest handle reqPair
      Logger.logInfo logH "Start-Message was sent."

sendKeyboard :: (Monad m, MonadThrow m) =>
                 Handle m ->
                 BotMessage.Message ->
                 m B.ByteString
sendKeyboard handle message = do
  let logH = hLogger handle
      dbH = hDb handle
      config = cReq handle
      api = Settings.botApi config
      b1 = BotButton.Button "1" "Pressed 1"
      b2 = BotButton.Button "2" "Pressed 2"
      b3 = BotButton.Button "3" "Pressed 3"
      b4 = BotButton.Button "4" "Pressed 4"
      b5 = BotButton.Button "5" "Pressed 5"
      buttons = [b1, b2, b3, b4, b5]
      question = Settings.botQuestion config
  reqPair <- setKeyboardMessage
    api dbH message buttons question
  Logger.logInfo logH "Keyboard was sent."
  makeRequest handle reqPair

sendCommands :: (Monad m, MonadThrow m) =>
                 Handle m ->
                 m ()
sendCommands handle = do
  let logH = hLogger handle
      dbH = hDb handle
      config = cReq handle
      api = Settings.botApi config
  reqPairM <- setCommands api dbH
  case reqPairM of
    Nothing -> Logger.logWarning logH "No exist commands for this API"
    Just reqPair -> do
      _ <- makeRequest handle reqPair
      Logger.logInfo logH "Bot commands were created."

saveUploadedDoc :: (MonadThrow m, Monad m) =>
                    Handle m ->
                    Text ->
                    m B.ByteString
saveUploadedDoc handle file = do
  let logH = hLogger handle
      dbH = hDb handle
      config = cReq handle
      api = Settings.botApi config
  reqPairM <- setUploadedDoc api dbH file
  case reqPairM of
    Nothing -> do
      Logger.logWarning logH "No needs in save docs for this API!"
      throwM E.UploadedDocError
    Just reqPair -> do
      Logger.logInfo logH "Doc was saved."
      makeRequest handle reqPair

updateMessage :: (MonadThrow m, Monad m) =>
                  Handle m ->
                  BotMessage.Message ->
                  m BotMessage.Message
updateMessage handle message = do
  let dbH = hDb handle
      config = cReq handle
      api = Settings.botApi config
  docsM <- extractDoc api dbH message
  case docsM of
    Nothing -> return message
    Just docs -> do
      newDocs <- mapM (updateDoc handle) docs
      changeMessage api dbH message newDocs

updateDoc :: (MonadThrow m, Monad m) =>
              Handle m ->
              BotDoc.Document ->
              m BotDoc.Document
updateDoc handle doc = do
  let logH = hLogger handle
      dbH = hDb handle
      config = cReq handle
      api = Settings.botApi config
  serverUp <- getUploadedServer handle doc
  downDocM <- downloadDoc api dbH doc serverUp
  case downDocM of
    Nothing -> do
      Logger.logError logH "File wasn't uploaded"
      return doc
    Just file -> do
      Logger.logInfo logH "File was uploaded"
      objUp <- saveUploadedDoc handle file
      changeDoc api dbH doc objUp

makeRequest :: (Monad m, MonadThrow m) =>
                Handle m ->
                BotReqPair.ReqPair ->
                m B.ByteString
makeRequest handle reqPair = do
  let logH = hLogger handle
      dbH = hDb handle
      config = cReq handle
      api = Settings.botApi config
  (apiHost, apiOptions) <- createRequest api dbH reqPair
  manager <- newManager handle tlsManagerSettings
  initialRequest <- HTTPClient.parseRequest $ T.unpack apiHost
  let request = initialRequest {
    HTTPClient.method = "POST",
    HTTPClient.requestBody = HTTPClient.RequestBodyLBS apiOptions,
    HTTPClient.requestHeaders = [ ( "Content-Type",
                         "application/json; charset=utf-8")
                      ]
  }
  response <- httpLbs handle request manager
  let codeResp = statusCode $ HTTPClient.responseStatus response
  if codeResp == 200
    then do
      Logger.logInfo logH "Successful request to api."
      return $ HTTPClient.responseBody response
    else do
      Logger.logWarning logH $ "Unsuccessful request to api with code: "
        <> BotUtil.convertValue codeResp
      throwM $ E.ConnectionError codeResp
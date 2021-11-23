module Bot.Request.Request where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Control.Monad.Catch as Catch
import qualified Network.HTTP.Client as HTTPClient
import Data.Text (Text)
import Control.Monad.Catch (MonadThrow, throwM)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.Convertible.Base (convert)

import qualified Bot.Exception as E
import qualified Bot.Logger.Logger as Logger
import qualified Bot.DB.DBQ as BotDBQ
import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Method as BotMethod
import qualified Bot.Objects.RequestOptions as BotReqOptions
import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Objects.Button as BotButton
import qualified Bot.Objects.Document as BotDoc
import qualified Bot.Settings as Settings
import qualified Bot.Util as BotUtil

data Handle m = Handle {
  hLogger :: Logger.Handle m,
  hDb :: BotDBQ.Handle m,
  cReq :: Settings.Config,
  hParser :: BotParser.Handle m,
  
  createRequest :: BotMethod.Method -> BotReqOptions.RequestOptions ->
                   m (Text, B.ByteString),
  setUploadedServer :: BotDoc.Document ->
                    m (Maybe (BotMethod.Method, BotReqOptions.RequestOptions)),
  setUploadedDoc :: Text ->
                    m (Maybe (BotMethod.Method, BotReqOptions.RequestOptions)),
  setGetServer :: m (Maybe (BotMethod.Method, BotReqOptions.RequestOptions)),
  setGetUpdate :: BotUpdate.Update ->
                  m (BotMethod.Method, BotReqOptions.RequestOptions),
  setEchoMessage :: BotMessage.Message ->
                    m (BotMethod.Method, BotReqOptions.RequestOptions),
  setHelpMessage :: BotMessage.Message -> Text ->
                    m (Maybe (BotMethod.Method, BotReqOptions.RequestOptions)),
  setStartMessage :: BotMessage.Message -> Text ->
                     m (Maybe (BotMethod.Method, BotReqOptions.RequestOptions)),
  setKeyboardMessage :: BotMessage.Message -> [BotButton.Button] ->
                        Text ->
                        m (BotMethod.Method, BotReqOptions.RequestOptions),
  setCommands :: m (Maybe (BotMethod.Method, BotReqOptions.RequestOptions)),

  downloadDoc :: BotDoc.Document -> B.ByteString -> m (Maybe Text),
  extractDoc :: BotMessage.Message -> m (Maybe [BotDoc.Document]),
  changeMessage :: BotMessage.Message -> [BotDoc.Document] ->
                   m BotMessage.Message,
  changeDoc :: BotDoc.Document -> B.ByteString -> m BotDoc.Document,

  newManager :: HTTPClient.ManagerSettings -> m HTTPClient.Manager,
  httpLbs :: HTTPClient.Request -> HTTPClient.Manager ->
             m (HTTPClient.Response B.ByteString)
}

getServer :: (MonadThrow m, Monad m) =>
              Handle m ->
              m B.ByteString
getServer handle = do
  let logH = hLogger handle
  methodAndOptM <- setGetServer handle
  case methodAndOptM of
    Nothing -> do
      Logger.logError logH "No needs in server parameters for this API"
      Catch.throwM E.GetServerError
    Just (method, serverOptions) -> do
      Logger.logInfo logH "Get server parameters for requests."
      makeRequest handle method serverOptions

getUploadedServer :: (MonadThrow m, Monad m) => Handle m ->
                      BotDoc.Document -> m B.ByteString
getUploadedServer handle doc = do
  let logH = hLogger handle
  methodAndOptM <- setUploadedServer handle doc
  case methodAndOptM of
    Nothing -> do
      Logger.logWarning logH "No needs uploaded server for this API"
      Catch.throwM E.UploadedServerError
    Just (method, serverOptions) -> do
      Logger.logInfo logH "Get Uploaded server parameters for upload file."
      makeRequest handle method serverOptions

getUpdate :: (Monad m, MonadThrow m) => Handle m ->
              BotUpdate.Update -> m B.ByteString
getUpdate handle update = do
  (method, updateOptions) <- setGetUpdate handle update
  makeRequest handle method updateOptions

sendEchoMessage :: (MonadThrow m, Monad m) => Handle m ->
                    BotMessage.Message -> m BotMessage.Message
sendEchoMessage handle message = do
  let logH = hLogger handle
  newMessage <- updateMessage handle message
  (method, echoMessageOptions) <- setEchoMessage handle newMessage
  _ <- makeRequest handle method echoMessageOptions
  Logger.logInfo logH "Echo-Message was forwarded."
  return newMessage

sendNEchoMessage :: (MonadThrow m, Monad m) => Handle m -> BotMessage.Message ->
                    BotSynonyms.RepNum -> m ()
sendNEchoMessage handle _ 0 = do
  let logH = hLogger handle
  Logger.logInfo logH "Echo-Messages were sent."
sendNEchoMessage handle message n = do
  _ <- sendEchoMessage handle message
  sendNEchoMessage handle message (n - 1)

sendHelpMessage :: (MonadThrow m, Monad m) => Handle m ->
                    BotMessage.Message ->
                    m (BotMethod.Method, BotReqOptions.RequestOptions)
sendHelpMessage handle message = do
  let logH = hLogger handle
      config = cReq handle
      helpText = Settings.botDescription config
  methodAndOptM <- setHelpMessage handle message (convert helpText)
  case methodAndOptM of
    Nothing -> do
      Logger.logWarning logH "No exist '/help' command for this API"
      Catch.throwM E.HelpMessageError
    Just (method, helpMessageOptions) -> do
      _ <- makeRequest handle method helpMessageOptions
      Logger.logInfo logH "Help-Message was sent."
      return (method, helpMessageOptions)

sendStartMessage :: (MonadThrow m, Monad m) => Handle m ->
                     BotMessage.Message -> m ()
sendStartMessage handle message = do
  let logH = hLogger handle
      startText = "You are welcome!"
  methodAndOptM <- setStartMessage handle message startText
  case methodAndOptM of
    Nothing -> do
      Logger.logWarning logH "No exist '/start' command for this API"
      Catch.throwM E.StartMessageError
    Just (method, startMessageOptions) -> do
      _ <- makeRequest handle method startMessageOptions
      Logger.logInfo logH "Start-Message was sent."

sendKeyboard :: (Monad m, MonadThrow m) => Handle m -> BotMessage.Message -> m B.ByteString
sendKeyboard handle message = do
  let logH = hLogger handle
      config = cReq handle
      b1 = BotButton.Button "1" "Pressed 1"
      b2 = BotButton.Button "2" "Pressed 2"
      b3 = BotButton.Button "3" "Pressed 3"
      b4 = BotButton.Button "4" "Pressed 4"
      b5 = BotButton.Button "5" "Pressed 5"
      buttons = [b1, b2, b3, b4, b5]
      question = Settings.botQuestion config
  (method, keyboardMessageOptions) <- setKeyboardMessage
    handle message buttons question
  Logger.logInfo logH "Keyboard was sent."
  makeRequest handle method keyboardMessageOptions

sendCommands :: (Monad m, MonadThrow m) => Handle m -> m ()
sendCommands handle = do
  let logH = hLogger handle
  methodAndOptM <- setCommands handle
  case methodAndOptM of
    Nothing -> Logger.logWarning logH "No exist commands for this API"
    Just (method, commandOptions) -> do
      _ <- makeRequest handle method commandOptions
      Logger.logInfo logH "Bot commands were created."

saveUploadedDoc :: (MonadThrow m, Monad m) => Handle m ->
                    Text -> m B.ByteString
saveUploadedDoc handle file = do
  let logH = hLogger handle
  methodAndOptM <- setUploadedDoc handle file
  case methodAndOptM of
    Nothing -> do
      Logger.logWarning logH "No needs in save docs for this API!"
      Catch.throwM E.UploadedDocError
    Just (method, docOptions) -> do
      Logger.logInfo logH "Doc was saved."
      makeRequest handle method docOptions

updateMessage :: (MonadThrow m, Monad m) => Handle m ->
                  BotMessage.Message -> m BotMessage.Message
updateMessage handle message = do
  docsM <- extractDoc handle message
  case docsM of
    Nothing -> return message
    Just docs -> do
      newDocs <- mapM (updateDoc handle) docs
      changeMessage handle message newDocs

updateDoc :: (MonadThrow m, Monad m) => Handle m ->
              BotDoc.Document -> m BotDoc.Document
updateDoc handle doc = do
  let logH = hLogger handle
  serverUp <- getUploadedServer handle doc
  downDocM <- downloadDoc handle doc serverUp
  case downDocM of
    Nothing -> do
      Logger.logError logH "File wasn't uploaded"
      return doc
    Just file -> do
      Logger.logInfo logH "File was uploaded"
      objUp <- saveUploadedDoc handle file
      changeDoc handle doc objUp

makeRequest :: (Monad m, MonadThrow m) => Handle m ->
                BotMethod.Method ->
                BotReqOptions.RequestOptions -> m B.ByteString
makeRequest handle botApiMethod botOptions = do
  let logH = hLogger handle
  (api, apiOptions) <- createRequest handle botApiMethod botOptions
  manager <- newManager handle tlsManagerSettings
  initialRequest <- HTTPClient.parseRequest $ T.unpack api
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
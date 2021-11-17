module Bot.Request.Request where

import qualified Data.ByteString.Lazy as B
import qualified Control.Monad.Catch as Catch
import Data.Text (Text)
import Control.Monad.Catch (MonadThrow)

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

data Handle m = Handle {
  hLogger :: Logger.Handle m,
  hDb :: BotDBQ.Handle m,
  cReq :: Settings.Config,
  hParser :: BotParser.Handle m,

  makeRequest :: BotMethod.Method -> BotReqOptions.RequestOptions ->
                 m B.ByteString,
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
                    m (BotMethod.Method, BotReqOptions.RequestOptions),
  setStartMessage :: BotMessage.Message -> Text ->
                     m (Maybe (BotMethod.Method, BotReqOptions.RequestOptions)),
  setKeyboardMessage :: BotMessage.Message -> [BotButton.Button] ->
                        BotSynonyms.Description ->
                        m (BotMethod.Method, BotReqOptions.RequestOptions),
  setCommands :: m (Maybe (BotMethod.Method, BotReqOptions.RequestOptions)),

  downloadDoc :: BotDoc.Document -> B.ByteString -> m (Maybe Text),
  extractDoc :: BotMessage.Message -> m (Maybe [BotDoc.Document]),
  changeMessage :: BotMessage.Message -> [BotDoc.Document] ->
                   m BotMessage.Message,
  changeDoc :: BotDoc.Document -> B.ByteString -> m BotDoc.Document
}

getServer :: (MonadThrow m, Monad m) => Handle m -> m B.ByteString
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

getUpdate :: Monad m => Handle m -> BotUpdate.Update -> m B.ByteString
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

sendHelpMessage :: Monad m => Handle m -> BotMessage.Message ->
                   m (BotMethod.Method, BotReqOptions.RequestOptions)
sendHelpMessage handle message = do
  let logH = hLogger handle
      config = cReq handle
      helpText = Settings.botDescription config
  (method, helpMessageOptions) <- setHelpMessage handle message helpText
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
      Logger.logWarning logH "No needs uploaded server for this API"
      Catch.throwM E.StartMessageError
    Just (method, startMessageOptions) -> do
      _ <- makeRequest handle method startMessageOptions
      Logger.logInfo logH "Start-Message was sent."

sendKeyboard :: Monad m => Handle m -> BotMessage.Message -> m B.ByteString
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

sendCommands :: Monad m => Handle m -> m ()
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
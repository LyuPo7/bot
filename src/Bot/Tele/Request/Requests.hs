{-# LANGUAGE OverloadedStrings #-}

module Bot.Tele.Request.Requests where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Control.Exception as Exc
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics ()
import Data.Aeson (encode)
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseStatus, responseBody, RequestBody(..), Request(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

import qualified Bot.Exception as E
import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import Bot.Tele.Request.RequestsSpec (Handle(..))
import Bot.Tele.Request.Data
import Bot.Tele.Parser.Data
import Bot.Util (convert)

withHandleIO :: Logger.Handle IO-> Settings.Config -> (Handle IO -> IO a) -> IO a
withHandleIO logger config f = do
  let handle = Handle logger config
  f handle

{-- | Request to api --}
makeRequest :: Handle IO -> TeleRequest -> RequestOptions -> IO B.ByteString
makeRequest handle teleRequest options = do
  let logh = hLogger handle
      config = configReq handle
      token = Settings.botToken config
      hostApi = Settings.getHost Settings.apiTele
      methodApi = getRequest teleRequest
      api = T.concat [hostApi, token, methodApi]
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ T.unpack api
  let request = initialRequest { 
    method = "POST",
    requestBody = RequestBodyLBS $ encode options,
    requestHeaders = [ ( "Content-Type",
                         "application/json; charset=utf-8")
                      ]
  }
  response <- httpLbs request manager
  let codeResp = statusCode $ responseStatus response
  if codeResp == 200
    then do
      Logger.logDebug logh $ "Successfull request to api."
      return $ responseBody response
    else do
      Logger.logDebug logh $ "Unsuccessfull request to api with code: " <> convert codeResp
      Exc.throwIO $ E.ConnectionError codeResp

{-- | Get update request --}
getUpdate :: Handle IO -> Maybe UpdateID -> IO L8.ByteString
getUpdate handle updateId  = do
  -- Get JSON data
  let updateOptions = createGetUpdates updateId
  makeRequest handle getUpdates updateOptions

{-- | Send text Message request --}
sendTextMessage :: Handle IO -> ChatID -> Text -> IO ()
sendTextMessage handle chatId text = do
  let logh = hLogger handle
      message = createSendMessage chatId text
  _ <- makeRequest handle sendMessage message
  Logger.logInfo logh $ "Message with text: " <> 
      text <> " was sended to chat with id: " <> 
      convert chatId

{-- | Copy Message request --}
sendEchoMessage :: Handle IO -> ChatID -> MessageID -> IO ()
sendEchoMessage handle chatId messageId = do
  let logh = hLogger handle
      message = createCopyMessage chatId messageId
  _ <- makeRequest handle copyMessage message
  Logger.logInfo logh $ "Echo-Message with id: " <>
      convert messageId <>
      " was forwarded to chat with id: " <> convert chatId

{-- | Copy(n times) Message request --}
sendNEchoMessage :: Handle  IO -> ChatID -> MessageID -> RepNum -> IO ()
sendNEchoMessage handle _ _ 0 = do
  let logh = hLogger handle
  Logger.logInfo logh "Echo-Messages were sended."
sendNEchoMessage handle chatId messageId n = do
  sendEchoMessage handle chatId messageId
  sendNEchoMessage handle chatId messageId (n - 1)

{-- | Send Message with InlineKeyboard --}
sendQueryNumber :: Handle IO -> ChatID -> Text -> IO L8.ByteString
sendQueryNumber handle chatId question = do
  let logh = hLogger handle
      b1 = createButton "1" "Pressed 1"
      b2 = createButton "2" "Pressed 2"
      b3 = createButton "3" "Pressed 3"
      b4 = createButton "4" "Pressed 4"
      b5 = createButton "5" "Pressed 5"
      markupIn = createKeyboard [[b1, b2, b3, b4, b5]]
      query = createQueryMessage chatId question markupIn
  Logger.logInfo logh $ "Question was sended to chat with id: " <> convert chatId
  makeRequest handle sendMessage query

{-- | St Bot Commands "/help", "/repeat--}
setCommands :: Handle IO -> IO ()
setCommands handle = do
  let logh = hLogger handle
      commands = createBotCommands
  _ <- makeRequest handle setBotCommands commands
  Logger.logInfo logh "Bot commands: '/help', '/repeat' were created."
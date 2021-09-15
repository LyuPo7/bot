{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Bot.Vk.Request.Requests where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Web.FormUrlEncoded as Url
import qualified Control.Exception as Exc
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromJust)
import GHC.Generics ()
import Data.Aeson ()
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseStatus, responseBody, Request(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode, statusCode)
import Network.HTTP.Types.Status ()
import Control.Monad ()
import qualified System.IO as SIO

import qualified Bot.Exception as E
import Bot.Vk.Request.RequestsSpec (Handle(..))
import Bot.Vk.Request.AttachSpec (attachmentsToQuery)
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import Bot.Vk.Parser.Data
import Bot.Vk.Request.Data
import Bot.Util (convert)

withHandleIO :: Logger.Handle IO -> Settings.Config -> ParserSpec.Handle IO -> (Handle IO -> IO a) -> IO a
withHandleIO logger config parserh f = do
  let handle = Handle logger config parserh
  f handle

getUpdate :: Handle IO -> Text -> Text -> Integer -> IO B.ByteString
getUpdate handle server key timeStamp = do
  let logh = hLogger handle
      api = T.unpack server <> "?act=a_check&key=" <> T.unpack key <> "&ts=" <> show timeStamp
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest api
  let request = initialRequest {
    method = "POST",
    requestHeaders = [ 
        ("Content-Type", "application/json; charset=utf-8")
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

makeRequest :: Handle IO -> VkRequest -> Text -> IO B.ByteString
makeRequest handle vkRequest queryOptions = do
  let logh = hLogger handle
      hostApi = Settings.getHost Settings.apiVk
      methodApi = getRequest vkRequest
      api = hostApi <> methodApi
      apiOpt = api <> "?" <> queryOptions
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ T.unpack apiOpt
  let request = initialRequest {
    method = "POST",
    requestHeaders = [ 
        ("Content-Type", "application/json; charset=utf-8")
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

getServer :: Handle IO -> IO B.ByteString
getServer handle = do
  let logh = hLogger handle
      config = configReq handle
      groupId = fromJust $ Settings.botGroupId config
      token = Settings.botToken config
      params = getPollServer groupId token Settings.vkVersion
      queryOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable params
  Logger.logInfo logh "Get server parameters for requests."
  makeRequest handle getLongPollServer queryOptions

sendHelpMessage :: Handle IO -> UserID -> IO ()
sendHelpMessage handle userId = do
  let logh = hLogger handle
      config = configReq handle
      description = Settings.botDescription config
      token = Settings.botToken config
      message = (defaultMessage userId token Settings.vkVersion) {sendMessag_message = description}
      queryOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable message
  _ <- makeRequest handle sendMessage queryOptions
  Logger.logInfo logh $ "Help message was sended to chat with id: " <> convert userId

sendEchoMessage :: Handle IO -> UserID -> Text -> Maybe [Attachment] -> Maybe Geo -> IO ()
sendEchoMessage handle userId text atts geo = do
  let logh = hLogger handle
      config = configReq handle
      token = Settings.botToken config
      geoCoords = T.unpack <$> fmap geo_coordinates geo
      lat = fmap (\x -> read x :: Double) (head <$> fmap words geoCoords)
      long = fmap (\x -> read x :: Double) (last <$> fmap words geoCoords)
      message = (defaultMessage userId token Settings.vkVersion) {
        sendMessag_message = text,
        sendMessag_attachment = attachmentsToQuery atts,
        sendMessag_stickerId = returnStickerId atts,
        sendMessag_lat = lat,
        sendMessag_long = long
        }
      queryOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable message
  _ <- makeRequest handle sendMessage queryOptions
  Logger.logInfo logh $ "Echo message was sended to chat with id: " <> convert userId

sendNEchoMessage :: Handle IO -> UserID -> Text -> Maybe [Attachment] -> Maybe Geo -> RepNum -> IO ()
sendNEchoMessage handle _ _ _ _ 0 = do
  let logh = hLogger handle
  Logger.logInfo logh "Echo-Messages were sended."
sendNEchoMessage logh userId text atts geo n = do
  sendEchoMessage logh userId text atts geo
  sendNEchoMessage logh userId text atts geo (n-1)

sendRepeatMessage :: Handle IO -> UserID -> IO ()
sendRepeatMessage handle userId = do
  let logh = hLogger handle
      config = configReq handle
      token = Settings.botToken config
      question = Settings.botQuestion config
  hf <- SIO.openFile "src/Bot/files/repeatButtons.txt" SIO.ReadMode
  keyboardF <- SIO.hGetLine hf
  let message = (defaultMessage userId token Settings.vkVersion) {sendMessag_message = question}
      queryOptions = T.pack $ L8.unpack (Url.urlEncodeAsFormStable message) <> "&keyboard=" <> keyboardF
  _ <- makeRequest handle sendMessage queryOptions
  Logger.logInfo logh $ "Repeat message was sended to chat with id: " <> convert userId

returnStickerId :: Maybe [Attachment] -> Maybe Integer
returnStickerId xsm = do
  xs <- xsm
  let awSticker = filter (\x -> attach_type x == "sticker") xs
  case awSticker of
    [x] -> sticker_id <$> attach_sticker x
    _ -> Nothing -- Sticker maybe only one in Message
    
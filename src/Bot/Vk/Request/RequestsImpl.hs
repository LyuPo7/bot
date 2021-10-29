{-# LANGUAGE OverloadedStrings #-}

module Bot.Vk.Request.RequestsImpl where

import qualified Data.ByteString.Lazy as B
import qualified Control.Exception as Exc
import qualified Data.Text as T
import Data.Text (Text)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode, statusCode)
import Network.HTTP.Client (Request(..),
                            newManager, parseRequest, httpLbs,
                            responseStatus, responseBody)

import qualified Bot.Exception as E
import Bot.Logger (Handle(..))
import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import Bot.Vk.Request.Data
import Bot.Util (convert)

getUpdate :: Handle IO -> Text -> Text -> Integer -> IO B.ByteString
getUpdate handle server key timeStamp = do
  let api = T.unpack server
        <> "?act=a_check&key="
        <> T.unpack key
        <> "&ts="
        <> show timeStamp
        <> "&wait="
        <> show Settings.timeout
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
      Logger.logDebug handle "Successfull request to api."
      return $ responseBody response
    else do
      Logger.logDebug handle $ "Unsuccessfull request to api with code: "
        <> convert codeResp
      Exc.throwIO $ E.ConnectionError codeResp

makeRequest :: Handle IO -> VkRequest -> Text -> IO B.ByteString
makeRequest handle vkRequest queryOptions = do
  let hostApi = Settings.getHost Settings.apiVk
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
      Logger.logDebug handle "Successfull request to api."
      return $ responseBody response
    else do
      Logger.logDebug handle $ "Unsuccessfull request to api with code: "
       <> convert codeResp
      Exc.throwIO $ E.ConnectionError codeResp
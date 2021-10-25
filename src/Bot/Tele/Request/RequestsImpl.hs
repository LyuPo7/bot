{-# LANGUAGE OverloadedStrings #-}

module Bot.Tele.Request.RequestsImpl where

import qualified Data.ByteString.Lazy as B
import qualified Control.Exception as Exc
import qualified Data.Text as T
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode, statusCode)
import Data.Aeson (encode)
import Network.HTTP.Client (Request(..), RequestBody(..), 
                            newManager, parseRequest, httpLbs,
                            responseStatus, responseBody)

import qualified Bot.Exception as E
import Bot.Tele.Parser.ParserSpec (Handle(..))
import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import Bot.Tele.Request.Data
import Bot.Util (convert)

{-- | Request to api --}
makeRequest :: Handle IO -> TeleRequest -> RequestOptions -> IO B.ByteString
makeRequest handle teleRequest options = do
  let logh = hLogger handle
      config = cPar handle
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
      Logger.logDebug logh "Successfull request to api."
      return $ responseBody response
    else do
      Logger.logDebug logh $ "Unsuccessfull request to api with code: "
        <> convert codeResp
      Exc.throwIO $ E.ConnectionError codeResp
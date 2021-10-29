module Bot.Vk.Request.DocumentImpl where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Web.FormUrlEncoded as Url
import qualified Data.Text as T
import Data.Text (Text)
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client.MultipartFormData
import qualified System.IO as SIO

import Bot.Vk.Request.RequestsSpec (Handle(..))
import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Vk.Request.RequestsImpl as ReqImpl
import qualified Bot.Vk.Request.Data as RD

getUploadedServer :: Handle IO -> Integer -> Text -> IO B.ByteString
getUploadedServer handle peerId fileType = do
  let logh = hLogger handle
      config = configReq handle
      token = Settings.botToken config
      query = RD.getLink peerId fileType Settings.vkVersion token
      queryOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable query
  ReqImpl.makeRequest logh RD.getMessagesUploadServer queryOptions

{- | Download a URL.  (Left errorMessage) if an error,
(Right doc) if success. -}
downloadFile :: Text -> FilePath -> IO ()
downloadFile link fileName = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest $ T.unpack link
  response <- httpLbs request manager
  file <- SIO.openBinaryFile fileName SIO.WriteMode
  SIO.hPutStr file (L8.unpack $ responseBody response)
  SIO.hClose file

uploadFile :: Text -> FilePath -> IO B.ByteString
uploadFile link fileName = do
  -- we fork the server in a separate thread and send a test
  -- request to it from the main thread.
  manager <- newManager tlsManagerSettings
  req <- parseRequest (T.unpack link)
  response <- flip httpLbs manager =<< formDataBody form req
  return $ responseBody response
  where form = [ partFileSource "file" fileName ]

saveUploadedDoc :: Handle IO -> Text -> IO B.ByteString
saveUploadedDoc handle file = do
  let logh = hLogger handle
      config = configReq handle
      token = Settings.botToken config
      link = RD.saveNewDoc file token Settings.vkVersion
  Logger.logInfo logh "Doc was saved."
  let queryOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable link
  ReqImpl.makeRequest logh RD.saveDoc queryOptions
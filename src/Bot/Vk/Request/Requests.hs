{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Bot.Vk.Request.Requests where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Web.FormUrlEncoded as Url
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromJust, fromMaybe)
import GHC.Generics ()
import Data.Aeson ()
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseStatus, responseBody, Request(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode, statusCode)
import Network.HTTP.Types.Status ()
import Network.HTTP.Client.MultipartFormData
import Control.Monad ()
import qualified System.IO as SIO
import System.Directory

import Bot.Vk.Request.RequestsSpec (Handle(..))
import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Vk.Parser.Parser as Parser
import Bot.Vk.Parser.Data
import Bot.Vk.Request.Data

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
  Logger.logInfo logh $ "The status code was: " <> convert (statusCode $ responseStatus response)
  return $ responseBody response

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
  Logger.logInfo logh $ "The status code was: " <> convert (statusCode $ responseStatus response)
  return $ responseBody response

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

getUploadedServer :: Handle IO -> Integer -> Text -> IO B.ByteString
getUploadedServer handle peerId fileType = do
  let config = configReq handle
      token = Settings.botToken config
      query = getLink peerId fileType Settings.vkVersion token
      queryOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable query
  makeRequest handle getMessagesUploadServer queryOptions

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
      link = saveNewDoc file token Settings.vkVersion
  Logger.logInfo logh "Doc was saved."
  let queryOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable link
  makeRequest handle saveDoc queryOptions

attachmentsToQuery :: Maybe [Attachment] -> Maybe Text
attachmentsToQuery Nothing = Nothing
attachmentsToQuery (Just a) = if null queryStringApi
  then Nothing
  else Just $ T.intercalate "," queryStringApi
  where queryStringApi = map attachmentToString a

attachmentToString :: Attachment -> Text
attachmentToString attach = case attach_type attach of 
    "photo" -> (attach_type attach) <> (convert (getPhotoOwnerId attach)) <> "_" <> (convert (getPhotoId attach)) <> "_" <> (getPhotoAccessKey attach)
    "video" -> (attach_type attach) <> (convert (getVideoOwnerId attach)) <> "_" <> (convert (getVideoId attach)) <> "_" <> (getVideoAccessKey attach)
    "audio" -> (attach_type attach) <> (convert (getAudioOwnerId attach)) <> "_" <> (convert (getAudioId attach))
    "doc" -> (attach_type attach) <> (convert (getDocOwnerId attach)) <> "_" <> (convert (getDocId attach))
    _ -> ""
    --"link" -> []
    --"market" -> (getMarketId attach, getMarketOwnerId attach, "")
    --"market_album" -> (getMarketAlbumId attach, getMarketAlbumOwnerId attach, "")
    --"wall" -> (getWallId attach, getWallOwnerId attach, "")
    --"wall_reply" -> (getWallReplyId attach, getWallReplyOwnerId attach, "")
    --"sticker" -> unpack $ attach_type attach <> unpack $ getStickerId attach <> "_" <> convert getStickerOwnerId attach
    --"gift" -> (getGiftId attach, getGiftOwnerId attach, "")

updateAttachments :: Handle IO -> Maybe [Attachment] -> IO (Maybe [Attachment])
updateAttachments handle Nothing = do
  let logh = hLogger handle
  Logger.logDebug logh "Message hasn't Attachments."
  return Nothing
updateAttachments handle (Just attachs) = do
  let logh = hLogger handle
  Logger.logDebug logh "Message has Attachments."
  Just <$> mapM (updateAttachment handle) attachs

updateAttachment :: Handle IO -> Attachment -> IO Attachment
updateAttachment handle attach = do
  let attachType = attach_type attach
  case attachType of
    "doc" -> do
      docNew <- updateDoc handle $ fromJust $ attach_doc attach
      return attach { attach_doc = Just docNew }
    _ -> return attach

-- work with Doc Attachment
updateDoc :: Handle IO -> Document -> IO Document
updateDoc handle doc = do
  let logh = hLogger handle
      parseh = hParser handle
      -- extract url from file
      link = document_url doc
      title = document_title doc
      userId = document_ownerId doc
  -- create temp directory
  tempDir <- getTemporaryDirectory
  (tempFileName, _) <- SIO.openTempFile tempDir (T.unpack title)
  -- download file
  downloadFile link tempFileName
  Logger.logInfo logh "File was downloaded"
  -- get server for upload file
  serverUp <- getUploadedServer handle userId (T.pack "doc")
  urlResp <- Parser.parseUploadUrl parseh serverUp
  let url = maybe (T.pack "") upUrl_uploadUrl (upUrlResponse_response urlResp)
  -- upload file
  fileUp <- uploadFile url tempFileName
  fileResp <- Parser.parseUploadFile parseh fileUp
  let file = fromMaybe (T.pack "") $ upFileResponse_file fileResp
  Logger.logInfo logh "File was uploaded"
  -- save file
  objUp <- saveUploadedDoc handle file
  obj <- Parser.parseUploadObject parseh objUp
  -- remove tempFile
  --removeDirectoryRecursive tempDir
  case upObjResponse_response obj of
    [x] -> do
      let urlNew = upObj_url x
          idNew = upObj_id x
          idOwnerNew = upObj_ownerId x
      Logger.logInfo logh "Doc changed"
      return doc {
        document_id = idNew,
        document_ownerId = idOwnerNew,
        document_url = urlNew
      }
    _ -> do
      Logger.logWarning logh "No uploaded objects" -- Maybe only one uploaded object
      return doc


returnStickerId :: Maybe [Attachment] -> Maybe Integer
returnStickerId Nothing = Nothing
returnStickerId (Just as) =
  case awSticker of
    [x] -> sticker_id <$> attach_sticker x
    _ -> Nothing -- Sticker maybe only one in Message
    where awSticker = filter (\a -> attach_type a == "sticker") as

-- get Attachment's Id
getPhotoId :: Attachment -> Integer
getPhotoId = photo_id . fromJust . attach_photo

getVideoId :: Attachment -> Integer
getVideoId = video_id . fromJust . attach_video

getAudioId :: Attachment -> Integer
getAudioId = audio_id . fromJust . attach_audio

getDocId :: Attachment -> Integer
getDocId = document_id . fromJust . attach_doc

--getLinkId :: Attachment -> Integer
--getLinkId = fromJust . fmap link_id . attach_link

getMarketId :: Attachment -> Integer
getMarketId = market_id . fromJust . attach_marketItem

getMarketAlbumId :: Attachment -> Integer
getMarketAlbumId = marketAl_id . fromJust . attach_marketCollection

getWallId :: Attachment -> Integer
getWallId = wall_id . fromJust . attach_wallPost

getWallReplyId :: Attachment -> Integer
getWallReplyId = wallRep_id . fromJust . attach_wallComment

--getGiftId :: Attachment -> Integer
--getGiftId = fromJust . fmap gift_id . attach_gift

getStickerId :: Attachment -> Integer
getStickerId = sticker_id . fromJust . attach_sticker

-- get Attachment's ownerId

getPhotoOwnerId :: Attachment -> Integer
getPhotoOwnerId = photo_ownerId . fromJust . attach_photo

getVideoOwnerId :: Attachment -> Integer
getVideoOwnerId = video_ownerId . fromJust . attach_video

getAudioOwnerId :: Attachment -> Integer
getAudioOwnerId = audio_ownerId . fromJust . attach_audio

getDocOwnerId :: Attachment -> Integer
getDocOwnerId = document_ownerId . fromJust . attach_doc

--getLinkOwnerId :: Attachment -> Integer
--getLinkOwnerId = fromJust . fmap link_id . attach_link

getMarketOwnerId :: Attachment -> Integer
getMarketOwnerId = market_ownerId . fromJust . attach_marketItem

getMarketAlbumOwnerId :: Attachment -> Integer
getMarketAlbumOwnerId = marketAl_ownerId . fromJust . attach_marketCollection

getWallOwnerId :: Attachment -> Integer
getWallOwnerId = wall_ownerId . fromJust . attach_wallPost

getWallReplyOwnerId :: Attachment -> Integer
getWallReplyOwnerId = wallRep_ownerId . fromJust . attach_wallComment

--getGiftOwnerId :: Attachment -> Integer
--getGiftOwnerId = fromJust . fmap gift_ownerId . attach_gift

getStickerOwnerId :: Attachment -> Integer
getStickerOwnerId = sticker_id . fromJust . attach_sticker

-- get Attachment's access_key
getPhotoAccessKey :: Attachment -> Text
getPhotoAccessKey = photo_accessKey . fromJust . attach_photo

getVideoAccessKey :: Attachment -> Text
getVideoAccessKey = video_accessKey . fromJust . attach_video

getDocAccessKey :: Attachment -> Text
getDocAccessKey = document_accessKey . fromJust . attach_doc

convert :: Show a => a -> Text
convert = T.pack . show

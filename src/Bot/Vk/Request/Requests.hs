{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Bot.Vk.Request.Requests where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Web.FormUrlEncoded as Url
import Data.Text (Text, unpack, pack)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (intercalate)
import GHC.Generics ()
import Data.Aeson ()
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseStatus, responseBody, Request(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode, statusCode)
import Network.HTTP.Types.Status ()
import Network.HTTP.Client.MultipartFormData
import Control.Monad ()
import System.IO
import System.Directory

import qualified Bot.Config as Config
import qualified Bot.Logger as BL
import Bot.Settings (apiVk, config, getHost, vkVersion)
import Bot.Vk.Request.Data
import Bot.Vk.Parser.Data
import Bot.Vk.Parser.Parser

getUpdate :: BL.Handle -> Text -> Text -> Integer -> IO B.ByteString
getUpdate logh server key timeStamp = do
  manager <- newManager tlsManagerSettings
  let api = unpack server ++ "?act=a_check&key=" ++ unpack key ++ "&ts=" ++ show timeStamp
  initialRequest <- parseRequest api
  let request = initialRequest {
    method = "POST",
    requestHeaders = [ 
        ("Content-Type", "application/json; charset=utf-8")
    ]
  }
  response <- httpLbs request manager
  BL.logInfo logh $ "The status code was: " ++ show (statusCode $ responseStatus response)
  return $ responseBody response

makeRequest :: BL.Handle -> VkRequest -> String -> IO B.ByteString
makeRequest logh vkRequest queryOptions = do
  manager <- newManager tlsManagerSettings
  --let hostApi = BC.unpack $ getHost apiVk
  let hostApi = BC.unpack $ getHost apiVk
  let methodApi = BC.unpack $ getRequest vkRequest
  let api = hostApi ++ methodApi
  let apiOpt = api ++ "?" ++ queryOptions
  initialRequest <- parseRequest apiOpt
  let request = initialRequest {
    method = "POST",
    requestHeaders = [ 
        ("Content-Type", "application/json; charset=utf-8")
    ]
  }
  response <- httpLbs request manager
  BL.logInfo logh $ "The status code was: " ++ show (statusCode $ responseStatus response)
  return $ responseBody response

getServer :: BL.Handle -> IO B.ByteString
getServer logh = do
  groupId <- fromJust <$> fmap Config.botGroupId config
  token <- fmap Config.botToken config
  let params = GetLongPollServer { 
    pollServer_groupId = groupId,
    pollServer_accessToken = token,
    pollServer_v = vkVersion
  }
  let queryOptions = L8.unpack $ Url.urlEncodeAsFormStable params
  BL.logInfo logh "Get server parameters for requests."
  makeRequest logh getLongPollServer queryOptions

sendHelpMessage :: BL.Handle -> Integer -> IO ()
sendHelpMessage logh userId = do
  description <- fmap Config.botDescription config
  token <- fmap Config.botToken config
  let message = SendMessage { 
    sendMessag_accessToken = token,
    sendMessag_userId = userId,
    sendMessag_message = description,
    sendMessag_v = vkVersion,
    sendMessag_attachment = Nothing,
    sendMessag_stickerId = Nothing,
    sendMessag_lat = Nothing,
    sendMessag_long = Nothing
  }
  let queryOptions = L8.unpack $ Url.urlEncodeAsFormStable message
  _ <- makeRequest logh sendMessage queryOptions
  BL.logInfo logh $ "Help message was sended to chat with id: " ++ show userId

sendEchoMessage :: BL.Handle -> Integer -> Text -> Maybe [Attachment] -> Maybe Geo -> IO ()
sendEchoMessage logh userId text atts geo = do
  token <- fmap Config.botToken config
  let geoCoords = unpack <$> fmap geo_coordinates geo
  let lat = fmap (\x -> read x :: Double) (head <$> fmap words geoCoords)
  let long = fmap (\x -> read x :: Double) (last <$> fmap words geoCoords)
  let message = SendMessage { 
    sendMessag_accessToken = token,
    sendMessag_userId = userId,
    sendMessag_message = text,
    sendMessag_v = "5.80",
    sendMessag_attachment = attachmentsToQuery atts,
    sendMessag_stickerId = returnStickerId atts,
    sendMessag_lat = lat,
    sendMessag_long = long
  }
  let queryOptions = L8.unpack $ Url.urlEncodeAsFormStable message
  _ <- makeRequest logh sendMessage queryOptions
  BL.logInfo logh $ "Echo message was sended to chat with id: " ++ show userId

sendNEchoMessage :: BL.Handle -> Integer -> Text -> Maybe [Attachment] -> Maybe Geo -> Integer -> IO ()
sendNEchoMessage logh _ _ _ _ 0 = BL.logInfo logh "Echo-Messages were sended."
sendNEchoMessage logh userId text atts geo n = do
  sendEchoMessage logh userId text atts geo
  sendNEchoMessage logh userId text atts geo (n-1)

sendRepeatMessage :: BL.Handle -> Integer -> IO ()
sendRepeatMessage logh userId = do
  token <- fmap Config.botToken config
  quetion <- fmap Config.botQuestion config
  hf <- openFile "src/Bot/files/repeatButtons.txt" ReadMode
  keyboardF <- hGetLine hf
  let message = SendMessage { 
    sendMessag_accessToken = token,
    sendMessag_userId = userId,
    sendMessag_message = quetion,
    sendMessag_v = vkVersion,
    sendMessag_attachment = Nothing,
    sendMessag_stickerId = Nothing,
    sendMessag_lat = Nothing,
    sendMessag_long = Nothing
  }
  let queryOptions = L8.unpack (Url.urlEncodeAsFormStable message) ++ "&keyboard=" ++ keyboardF
  _ <- makeRequest logh sendMessage queryOptions
  BL.logInfo logh $ "Repeat message was sended to chat with id: " ++ show userId

-- id = 663743323, type = "doc"
getUploadedServer :: BL.Handle -> Integer -> Text -> IO B.ByteString
getUploadedServer logh peerId fileType = do
  token <- fmap Config.botToken config
  let query = GetUploadLink {
    getUplLink_peerId = peerId,
    getUplLink_type = fileType,
    getUplLink_v = vkVersion,
    getUplLink_accessToken = token
  }
  let queryOptions = L8.unpack $ Url.urlEncodeAsFormStable query
  makeRequest logh getMessagesUploadServer queryOptions

{- | Download a URL.  (Left errorMessage) if an error,
(Right doc) if success. -}
downloadFile :: String -> FilePath -> IO ()
downloadFile link fileName = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest link
  response <- httpLbs request manager
  file <- openBinaryFile fileName WriteMode
  hPutStr file (L8.unpack $ responseBody response)
  hClose file

uploadFile :: String -> FilePath -> IO B.ByteString
uploadFile link fileName = do
  -- we fork the server in a separate thread and send a test
  -- request to it from the main thread.
  manager <- newManager tlsManagerSettings
  req <- parseRequest link
  response <- flip httpLbs manager =<< formDataBody form req
  return $ responseBody response
  where form =
          [ --partBS "title" "World",
            --partBS "text" $ encodeUtf8 "Hello",
            partFileSource "file" fileName
          ]

saveUploadedDoc :: BL.Handle -> Text ->  IO B.ByteString
saveUploadedDoc logh file = do
  token <- fmap Config.botToken config
  let link = SaveDoc {
    saveDocSer_file = file,
    saveDocSer_accessToken = token,
    saveDocSer_v = vkVersion
  }
  BL.logInfo logh "Doc was saved."
  let queryOptions = L8.unpack $ Url.urlEncodeAsFormStable link
  makeRequest logh saveDoc queryOptions

attachmentsToQuery :: Maybe [Attachment] -> Maybe Text
attachmentsToQuery Nothing = Nothing
attachmentsToQuery (Just a) = if null queryStringApi
  then Nothing
  else Just $ pack $ intercalate "," queryStringApi
  where queryStringApi = map attachmentToString a

attachmentToString :: Attachment -> String
attachmentToString attach = case attach_type attach of 
    "photo" -> unpack (attach_type attach) ++ show (getPhotoOwnerId attach) ++ "_" ++ show (getPhotoId attach) ++ "_" ++ unpack (getPhotoAccessKey attach)
    "video" -> unpack (attach_type attach) ++ show (getVideoOwnerId attach) ++ "_" ++ show (getVideoId attach) ++ "_" ++ unpack (getVideoAccessKey attach)
    "audio" -> unpack (attach_type attach) ++ show (getAudioOwnerId attach) ++ "_" ++ show (getAudioId attach)
    "doc" -> unpack (attach_type attach) ++ show (getDocOwnerId attach) ++ "_" ++ show (getDocId attach)
    _ -> []
    --"link" -> []
    --"market" -> (getMarketId attach, getMarketOwnerId attach, "")
    --"market_album" -> (getMarketAlbumId attach, getMarketAlbumOwnerId attach, "")
    --"wall" -> (getWallId attach, getWallOwnerId attach, "")
    --"wall_reply" -> (getWallReplyId attach, getWallReplyOwnerId attach, "")
    --"sticker" -> unpack $ attach_type attach ++ unpack $ getStickerId attach ++ "_" ++ show getStickerOwnerId attach
    --"gift" -> (getGiftId attach, getGiftOwnerId attach, "")

updateAttachments :: BL.Handle -> Maybe [Attachment] -> IO (Maybe [Attachment])
updateAttachments logh Nothing = do
  BL.logDebug logh "Message hasn't Attachments."
  return Nothing
updateAttachments logh (Just attachs) = do
  BL.logDebug logh "Message has Attachments."
  Just <$> mapM (updateAttachment logh) attachs

updateAttachment :: BL.Handle -> Attachment -> IO Attachment
updateAttachment logh attach = do
  let attachType = attach_type attach
  case attachType of
    "doc" -> do
      docNew <- updateDoc logh $ fromJust $ attach_doc attach
      return attach { attach_doc = Just docNew }
    _ -> return attach

-- work with Doc Attachment
updateDoc :: BL.Handle -> Document -> IO Document
updateDoc logh doc = do
  -- extract url from file
  let link = document_url doc
  let title = document_title doc
  let userId = document_ownerId doc
  -- create temp directory
  tempDir <- getTemporaryDirectory
  (tempFileName, _) <- openTempFile tempDir (unpack title)
  -- download file
  downloadFile (unpack link) tempFileName
  BL.logInfo logh "File was downloaded"
  -- get server for upload file
  urlResp <- parseUploadUrl logh $ getUploadedServer logh userId (pack "doc")
  let url = maybe (pack "") upUrl_uploadUrl (upUrlResponse_response urlResp)
  -- upload file
  fileResp <- parseUploadFile logh $ uploadFile (unpack url) tempFileName
  let file = fromMaybe (pack "") $ upFileResponse_file fileResp
  BL.logInfo logh "File was uploaded"
  -- save file
  obj <- parseUploadObject logh $ saveUploadedDoc logh file
  -- remove tempFile
  --removeDirectoryRecursive tempDir
  case upObjResponse_response obj of
    [x] -> do
      let urlNew = upObj_url x
      let idNew = upObj_id x
      let idOwnerNew = upObj_ownerId x
      BL.logInfo logh "Doc changed"
      return doc {
        document_id = idNew,
        document_ownerId = idOwnerNew,
        document_url = urlNew
      }
    _ -> do
      BL.logWarning logh "No uploaded objects" -- Maybe only one uploaded object
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


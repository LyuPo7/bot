{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Vk.Requests where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Web.FormUrlEncoded as Url
import Data.Text (Text, unpack, pack)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (intercalate)
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics (Generic)
import Data.Aeson.Types
import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status  (statusCode)
import Network.HTTP.Client.MultipartFormData
import Control.Monad (liftM)
import System.IO
import System.Directory

import Settings (apiVk, config, getHost, vkVersion)
import Config
import Vk.Types
import Vk.Parser
import Logger
import LoggerIO

-- data for RequestOptions requests
newtype VkRequest = VkRequest { getRequest :: BC.ByteString }

data GetLongPollServer = GetLongPollServer {
    pollServer_groupId :: Integer,
    pollServer_accessToken :: Text,
    pollServer_v :: Text
} deriving (Generic)

instance FromJSON GetLongPollServer where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance ToJSON GetLongPollServer where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.FromForm GetLongPollServer where
  fromForm = Url.genericFromForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.ToForm GetLongPollServer where
  toForm = Url.genericToForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

data LongPollServer = LongPollServer {
    longServer_key :: Text,
    longServer_ts :: Integer,
    longServer_act :: Text
} deriving (Generic)

instance FromJSON LongPollServer where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance ToJSON LongPollServer where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.FromForm LongPollServer where
  fromForm = Url.genericFromForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.ToForm LongPollServer where
  toForm = Url.genericToForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

data SendMessage = SendMessage {
    sendMessag_accessToken :: Text,
    sendMessag_userId :: Integer,
    sendMessag_message :: Text,
    sendMessag_v :: Text,
    sendMessag_attachment :: Maybe Text,
    sendMessag_stickerId :: Maybe Integer,
    sendMessag_lat :: Maybe Double,
    sendMessag_long :: Maybe Double
} deriving (Generic)

instance FromJSON SendMessage where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance ToJSON SendMessage where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.FromForm SendMessage where
  fromForm = Url.genericFromForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.ToForm SendMessage where
  toForm = Url.genericToForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

data GetUploadLink = GetUploadLink {
    getUplLink_type :: Text,
    getUplLink_peerId :: Integer,
    getUplLink_accessToken :: Text,
    getUplLink_v :: Text
} deriving (Generic)

instance FromJSON GetUploadLink where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance ToJSON GetUploadLink where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.FromForm GetUploadLink where
  fromForm = Url.genericFromForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.ToForm GetUploadLink where
  toForm = Url.genericToForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

data SaveDoc = SaveDoc {
    saveDocSer_file :: Text,
    saveDocSer_accessToken :: Text,
    saveDocSer_v :: Text
} deriving (Generic)

instance FromJSON SaveDoc where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance ToJSON SaveDoc where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.FromForm SaveDoc where
  fromForm = Url.genericFromForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.ToForm SaveDoc where
  toForm = Url.genericToForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

-- Poll
data CreatePoll = CreatePoll {
  createPoll_isMultiple :: Maybe Integer, -- 
  createPoll_question :: Text, -- Poll question, 1-300 characters.
  createPoll_answers :: [Text], -- List of poll options.
  createPoll_accessToken :: Text,
  createPoll_v :: Text
  } deriving (Show,Generic) 

instance FromJSON CreatePoll where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance ToJSON CreatePoll where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.FromForm CreatePoll where
  fromForm = Url.genericFromForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

instance Url.ToForm CreatePoll where
  toForm = Url.genericToForm Url.FormOptions {
    Url.fieldLabelModifier = camelTo2 '_' . drop 11 }

-- Vk Requests
getLongPollServer :: VkRequest
getLongPollServer = VkRequest "groups.getLongPollServer"

sendMessage :: VkRequest
sendMessage = VkRequest "messages.send"

getMessagesUploadServer :: VkRequest
getMessagesUploadServer = VkRequest "docs.getMessagesUploadServer"

saveDoc :: VkRequest
saveDoc = VkRequest "docs.save"

createPoll :: VkRequest
createPoll = VkRequest "polls.create"

getUpdate :: Logger.Handle -> Text -> Text -> Integer -> IO B.ByteString
getUpdate logh server key ts = do
  manager <- newManager tlsManagerSettings
  let api = unpack server ++ "?act=a_check&key=" ++ unpack key ++ "&ts=" ++ show ts
  initialRequest <- parseRequest api
  let request = initialRequest {
    method = "POST",
    requestHeaders = [ 
        ("Content-Type", "application/json; charset=utf-8")
    ]
  }
  response <- httpLbs request manager
  logInfo logh $ "The status code was: " ++ show (statusCode $ responseStatus response)
  return $ responseBody response

makeRequest :: Logger.Handle -> VkRequest -> String -> IO B.ByteString
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
  logInfo logh $ "The status code was: " ++ show (statusCode $ responseStatus response)
  return $ responseBody response

getServer :: Logger.Handle -> IO B.ByteString
getServer logh = do
  groupId <- fromJust <$> fmap botGroupId config
  token <- fmap botToken config
  let params = GetLongPollServer { 
    pollServer_groupId = groupId,
    pollServer_accessToken = token,
    pollServer_v = vkVersion
  }
  let queryOptions = L8.unpack $ Url.urlEncodeAsFormStable params
  logInfo logh "Get server parameters for requests."
  makeRequest logh getLongPollServer queryOptions

sendHelpMessage :: Logger.Handle -> Integer -> IO ()
sendHelpMessage logh userId = do
  description <- fmap botDescription config
  token <- fmap botToken config
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
  logInfo logh $ "Help message was sended to chat with id: " ++ show userId

sendEchoMessage :: Logger.Handle -> Integer -> Text -> Maybe [Attachment] -> Maybe Geo -> IO ()
sendEchoMessage logh userId text atts geo = do
  token <- fmap botToken config
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
  logInfo logh $ "Echo message was sended to chat with id: " ++ show userId

sendNEchoMessage :: Logger.Handle -> Integer -> Text -> Maybe [Attachment] -> Maybe Geo -> Integer -> IO ()
sendNEchoMessage logh userId text atts geo 0 = logInfo logh "Echo-Messages were sended."
sendNEchoMessage logh userId text atts geo n = do
  sendEchoMessage logh userId text atts geo
  sendNEchoMessage logh userId text atts geo (n-1)


sendRepeatMessage :: Logger.Handle -> Integer -> IO ()
sendRepeatMessage logh userId = do
  token <- fmap botToken config
  quetion <- fmap botQuestion config
  hf <- openFile "src/files/repeatButtons.txt" ReadMode
  keyboard <- hGetLine hf
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
  let queryOptions = L8.unpack (Url.urlEncodeAsFormStable message) ++ "&keyboard=" ++ keyboard
  _ <- makeRequest logh sendMessage queryOptions
  logInfo logh $ "Repeat message was sended to chat with id: " ++ show userId

-- id = 663743323, type = "doc"
getUploadedServer :: Logger.Handle -> Integer -> Text -> IO B.ByteString
getUploadedServer logh id fileType = do
  token <- fmap botToken config
  let query = GetUploadLink {
    getUplLink_peerId = id,
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

saveUploadedDoc :: Logger.Handle -> Text ->  IO B.ByteString
saveUploadedDoc logh file = do
  token <- fmap botToken config
  let link = SaveDoc {
    saveDocSer_file = file,
    saveDocSer_accessToken = token,
    saveDocSer_v = vkVersion
  }
  logInfo logh "Doc was saved."
  let queryOptions = L8.unpack $ Url.urlEncodeAsFormStable link
  makeRequest logh saveDoc queryOptions

attachmentsToQuery :: Maybe [Attachment] -> Maybe Text
attachmentsToQuery Nothing = Nothing
attachmentsToQuery (Just a) = if null queryString
  then Nothing
  else Just $ pack $ intercalate "," queryString
  where queryString = map attachmentToString a

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

updateAttachments :: Logger.Handle -> Maybe [Attachment] -> IO (Maybe [Attachment])
updateAttachments logh Nothing = do
  logDebug logh "Message hasn't Attachments."
  return Nothing
updateAttachments logh (Just attachs) = do
  logDebug logh "Message has Attachments."
  Just <$> mapM (updateAttachment logh) attachs

updateAttachment :: Logger.Handle -> Attachment -> IO Attachment
updateAttachment logh attach = do
  let attachType = attach_type attach
  case attachType of
    "doc" -> do
      docNew <- updateDoc logh $ fromJust $ attach_doc attach
      return attach { attach_doc = Just docNew }
    _ -> return attach

-- work with Doc Attachment
updateDoc :: Logger.Handle -> Document -> IO Document
updateDoc logh doc = do
  -- extract url from file
  let link = document_url doc
  let title = document_title doc
  let userId = document_ownerId doc
  -- create temp directory
  tempDir <- getTemporaryDirectory
  (tempFileName, tempFileHandle) <- openTempFile tempDir (unpack title)
  -- download file
  downloadFile (unpack link) tempFileName
  logInfo logh "File was downloaded"
  -- get server for upload file
  urlResp <- parseUploadUrl logh $ getUploadedServer logh userId (pack "doc")
  let url = maybe (pack "") upUrl_uploadUrl (upUrlResponse_response urlResp)
  -- upload file
  fileResp <- parseUploadFile logh $ uploadFile (unpack url) tempFileName
  let file = fromMaybe (pack "") $ upFileResponse_file fileResp
  logInfo logh "File was uploaded"
  -- save file
  obj <- parseUploadObject logh $ saveUploadedDoc logh file
  -- remove tempFile
  --removeDirectoryRecursive tempDir
  case upObjResponse_response obj of
    [] -> do
      logWarning logh "No uploaded objects"
      return doc
    [x] -> do
      let urlNew = upObj_url x
      let idNew = upObj_id x
      let idOwnerNew = upObj_ownerId x
      logInfo logh "Doc changed"
      return doc {
        document_id = idNew,
        document_ownerId = idOwnerNew,
        document_url = urlNew
      }

returnStickerId :: Maybe [Attachment] -> Maybe Integer
returnStickerId Nothing = Nothing
returnStickerId (Just as) =
  case awSticker of
    [x] -> sticker_id <$> attach_sticker x
    [] -> Nothing
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

{- 1. poll request (Возвращает данные для подключения к Bots Longpoll API. )
--https://api.vk.com/method/groups.getLongPollServer?group_id=205828081&access_token=e898d06d42ad252127024a7bb6c9b02ec53e4e6ed3d6e257ebadcf0b3b74cb9648a51d1eee69965faad63&v=5.80

-- return {
    key: 	"d0c8b4b8b806b4153912348f6c585542642ba2cc"
    server: "https://lp.vk.com/wh205828081"
    ts: "2"
    }

- 2. {$server}?act=a_check&key={$key}&ts={$ts}&wait=25 
https://lp.vk.com/wh205828081?act=a_check&key=60915c4e89beaa22d2c7051d55c0cc971386a237&ts=2&wait=25
-- return {
    ts: "2"
    updates: []
}

- 3. send Message
https://api.vk.com/method/messages.send?message=%22Hi%20from%20bot!%22&user_id=663743323&access_token=e898d06d42ad252127024a7bb6c9b02ec53e4e6ed3d6e257ebadcf0b3b74cb9648a51d1eee69965faad63&v=5.80

-4. send Message with Attachment [Photo]
https://api.vk.com/method/messages.send?message=%22Hi%20from%20bot!%22&user_id=663743323&access_token=e898d06d42ad252127024a7bb6c9b02ec53e4e6ed3d6e257ebadcf0b3b74cb9648a51d1eee69965faad63&v=5.131&attachment=photo663743323_457239018_314a755f475662ed9a&random_id=101

- 5. send Message with Attachment [Audio]
https://api.vk.com/method/messages.send?message=%22Hi%20from%20bot!%22&user_id=663743323&access_token=e898d06d42ad252127024a7bb6c9b02ec53e4e6ed3d6e257ebadcf0b3b74cb9648a51d1eee69965faad63&v=5.131&attachment=audio371745430_456343414

- 6. send Message with Attachment [Doc]
https://api.vk.com/method/messages.send?message=%22Hi%20from%20bot!%22&user_id=663743323&access_token=e898d06d42ad252127024a7bb6c9b02ec53e4e6ed3d6e257ebadcf0b3b74cb9648a51d1eee69965faad63&v=5.131&attachment=doc663743323_603680865

- 7. docs.getMessagesUploadServer (get Url for Upload doc to Private Message)
https://api.vk.com/method/docs.getMessagesUploadServer?type=doc&peer_id=663743323&access_token=e898d06d42ad252127024a7bb6c9b02ec53e4e6ed3d6e257ebadcf0b3b74cb9648a51d1eee69965faad63&v=5.80

- 8. docs.save
-}

pollServerOpt = GetLongPollServer {
    pollServer_groupId = 205828081,
    pollServer_accessToken = "e898d06d42ad252127024a7bb6c9b02ec53e4e6ed3d6e257ebadcf0b3b74cb9648a51d1eee69965faad63",
    pollServer_v = "5.80"
}

lp = LongPollServer {
    longServer_key = "60915c4e89beaa22d2c7051d55c0cc971386a237",
    longServer_ts = 2,
    longServer_act = "a_check"
}

helpM = SendMessage { 
    sendMessag_accessToken = "e898d06d42ad252127024a7bb6c9b02ec53e4e6ed3d6e257ebadcf0b3b74cb9648a51d1eee69965faad63",
    sendMessag_userId = 663743323,
    sendMessag_message = "Hi from bot!",
    sendMessag_v = "5.80",
    sendMessag_attachment = Nothing,
    sendMessag_stickerId = Nothing,
    sendMessag_lat = Nothing,
    sendMessag_long = Nothing
  }

echoM = SendMessage { 
    sendMessag_accessToken = "e898d06d42ad252127024a7bb6c9b02ec53e4e6ed3d6e257ebadcf0b3b74cb9648a51d1eee69965faad63",
    sendMessag_userId = 663743323,
    sendMessag_message = "Echo message:",
    sendMessag_v = "5.80",
    sendMessag_attachment = attachmentsToQuery $ Just [att1, att0],
    sendMessag_stickerId = Nothing,
    sendMessag_lat = Nothing,
    sendMessag_long = Nothing
  }

uplink = GetUploadLink {
  getUplLink_peerId = 663743323,
  getUplLink_type = "doc",
  getUplLink_v = "5.80",
  getUplLink_accessToken = "e898d06d42ad252127024a7bb6c9b02ec53e4e6ed3d6e257ebadcf0b3b74cb9648a51d1eee69965faad63"
}

link = SaveDoc {
    saveDocSer_accessToken = "e898d06d42ad252127024a7bb6c9b02ec53e4e6ed3d6e257ebadcf0b3b74cb9648a51d1eee69965faad63",
    saveDocSer_file = "663743323|0|-1|537232|7ccf7dda3c|log|1439371|test.log|f5fbd9af9cbc515d99a3cef496d8a174|c7e8a5ad1bce178292d20e6aecdeec53||||eyJkaXNrIjoiNDIiLCJwZWVyX3NlbmRlciI6Ii0yMDU4MjgwODEifQ=='\'",
    saveDocSer_v = "5.80"
  }
  
att0 = Attachment {
    attach_type = "audio", -- type of attachment.
    attach_photo = Nothing, -- 
    attach_video = Nothing,
    attach_audio = Just au0,
    attach_doc = Nothing,
    attach_link = Nothing,
    attach_marketItem = Nothing,
    attach_marketCollection = Nothing,
    attach_wallPost = Nothing,
    attach_wallComment = Nothing,
    attach_sticker = Nothing,
    attach_gift = Nothing
}

au0 = Audio {
  audio_id = 10000, -- Audio ID.
  audio_ownerId = 10000000, -- Audio owner ID.
  audio_title = "Text", -- Audio title.
  audio_artist = "Artist", -- Artist name.
  audio_duration = 100, -- Duration (in seconds).
  audio_url = "/df/"
}

att1 = Attachment {
    attach_type = "doc", -- type of attachment.
    attach_photo = Nothing, -- 
    attach_video = Nothing,
    attach_audio = Nothing,
    attach_doc = Just doc0,
    attach_link = Nothing,
    attach_marketItem = Nothing,
    attach_marketCollection = Nothing,
    attach_wallPost = Nothing,
    attach_wallComment = Nothing,
    attach_sticker = Nothing,
    attach_gift = Nothing
}

att2 = Attachment {
    attach_type = "sticker", -- type of attachment.
    attach_photo = Nothing, -- 
    attach_video = Nothing,
    attach_audio = Nothing,
    attach_doc = Nothing,
    attach_link = Nothing,
    attach_marketItem = Nothing,
    attach_marketCollection = Nothing,
    attach_wallPost = Nothing,
    attach_wallComment = Nothing,
    attach_sticker = Just sticker0,
    attach_gift = Nothing
}

ph0 = Photo { --photo663743323_457239018_314a755f475662ed9a
  photo_id = 457239018, -- Photo ID.
  photo_albumId = 3211, -- Photo album ID.
  photo_ownerId = 663743323, -- Photo owner ID.
  photo_userId = Nothing, -- ID of the user who uploaded the photo (if the photo is uploaded in community).
  photo_text = "", -- Description text.
  photo_date = 1111111, -- Date when the photo has been added in Unixtime. 
  photo_width = 120, -- Width of the original photo in px. 
  photo_height = 50, -- Height of the original photo in px.
  photo_accessKey = "314a755f475662ed9a"
}

doc0 = Document {
  document_id = 603680865, -- Document ID.
  document_ownerId = 663743323, -- Document owner ID.
  document_title = "geckodriver.log", -- Document title.
  document_size = 1439371, -- Document size.
  document_ext = "log", -- Document extension.
  document_date = 1626248757, -- Date when the document has been added in Unixtime.
  document_duration = Just 12, -- Duration (in seconds).
  document_url = "https://vk.com/doc663743323_603680865?hash=9bcdfb2af7d7908a15&dl=GY3DGNZUGMZTEMY:1626762521:ce54656ecd1f72cbaa&api=1&no_preview=1", -- Link to file. Note that links are bound to an IP address.
  document_accessKey = "ba3bf8fbf9e532aa0b" -- 
}

sticker0 = Sticker {
  sticker_id = 9019
}

keyboard0 = Keyboard {
  keyboard_buttons = [[but0]],
  keyboard_inline = Just False,
  keyboard_oneTime = Just True
}

but0 = Button {
  button_action = action0,
  button_color = Just "primary"
}

action0 = Action {
  action_type = "callback",
  action_label = "1",
  action_payload = "{\"button\": \"1\"}"
}
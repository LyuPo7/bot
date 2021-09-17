{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Bot.Vk.Request.RequestsSpec where

import Prelude hiding (readFile)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Web.FormUrlEncoded as Url
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromJust)

import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import Bot.Vk.Parser.Data
import Bot.Vk.Request.Data
import Bot.Util (convert)

data Handle m = Handle {
    hLogger :: Logger.Handle m,
    configReq :: Settings.Config,
    hParser :: ParserSpec.Handle m,
    
    readFile :: FilePath -> m String,
    makeRequest :: VkRequest -> Text -> m B.ByteString,
    getUpdate :: Text -> Text -> Integer -> m B.ByteString
}

getServer :: Monad m => Handle m -> m B.ByteString
getServer handle = do
  let logh = hLogger handle
      config = configReq handle
      groupId = fromJust $ Settings.botGroupId config
      token = Settings.botToken config
      params = getPollServer groupId token Settings.vkVersion
      queryOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable params
  Logger.logInfo logh "Get server parameters for requests."
  makeRequest handle getLongPollServer queryOptions

sendHelpMessage :: Monad m => Handle m -> UserID -> m ()
sendHelpMessage handle userId = do
  let logh = hLogger handle
      config = configReq handle
      description = Settings.botDescription config
      token = Settings.botToken config
      message = (defaultMessage userId token Settings.vkVersion) {sendMessag_message = description}
      queryOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable message
  _ <- makeRequest handle sendMessage queryOptions
  Logger.logInfo logh $ "Help message was sended to chat with id: " <> convert userId

sendEchoMessage :: Monad m => Handle m -> UserID -> Text -> Maybe [Attachment] -> Maybe Geo -> m ()
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

sendNEchoMessage :: Monad m => Handle m -> UserID -> Text -> Maybe [Attachment] -> Maybe Geo -> RepNum -> m ()
sendNEchoMessage handle _ _ _ _ 0 = do
  let logh = hLogger handle
  Logger.logInfo logh "Echo-Messages were sended."
sendNEchoMessage logh userId text atts geo n = do
  sendEchoMessage logh userId text atts geo
  sendNEchoMessage logh userId text atts geo (n-1)

sendRepeatMessage :: Monad m => Handle m -> UserID -> m ()
sendRepeatMessage handle userId = do
  let logh = hLogger handle
      config = configReq handle
      token = Settings.botToken config
      question = Settings.botQuestion config
  --hf <- SIO.openFile "src/Bot/files/repeatButtons.txt" SIO.ReadMode
  --keyboardF <- SIO.hGetLine hf
  keyboardF <- readFile handle "src/Bot/files/repeatButtons.txt"
  let message = (defaultMessage userId token Settings.vkVersion) {sendMessag_message = question}
      queryOptions = T.pack $ L8.unpack (Url.urlEncodeAsFormStable message) <> "&keyboard=" <> keyboardF
  _ <- makeRequest handle sendMessage queryOptions
  Logger.logInfo logh $ "Repeat message was sended to chat with id: " <> convert userId

attachmentsToQuery :: Maybe [Attachment] -> Maybe Text
attachmentsToQuery Nothing = Nothing
attachmentsToQuery (Just xs) = if null queryStringApi
  then Nothing
  else Just $ T.intercalate "," queryStringApi
  where queryStringApi = filter (not . T.null) (map attachmentToString xs)

attachmentToString :: Attachment -> Text
attachmentToString attach = case attach_type attach of 
    "photo" -> (attach_type attach) <> (convert (getPhotoOwnerId attach)) <> "_" <> (convert (getPhotoId attach)) <> "_" <> (getPhotoAccessKey attach)
    "video" -> (attach_type attach) <> (convert (getVideoOwnerId attach)) <> "_" <> (convert (getVideoId attach)) <> "_" <> (getVideoAccessKey attach)
    "audio" -> (attach_type attach) <> (convert (getAudioOwnerId attach)) <> "_" <> (convert (getAudioId attach))
    "doc" -> (attach_type attach) <> (convert (getDocOwnerId attach)) <> "_" <> (convert (getDocId attach))
    _ -> ""

returnStickerId :: Maybe [Attachment] -> Maybe Integer
returnStickerId xsm = do
  xs <- xsm
  let awSticker = filter (\x -> attach_type x == "sticker") xs
  case awSticker of
    [x] -> sticker_id <$> attach_sticker x
    _ -> Nothing -- Sticker maybe only one in Message

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

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Bot.Vk.Request.AttachSpec where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromJust)

import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Logger as Logger
import Bot.Vk.Parser.Data
import Bot.Util (convert)

data Handle m = Handle {
    hLogger :: Logger.Handle m,
    hReq :: ReqSpec.Handle m,
    hParser :: ParserSpec.Handle m,

    updateDoc :: Document -> m Document
}

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

updateAttachments :: Monad m => Handle m -> Maybe [Attachment] -> m (Maybe [Attachment])
updateAttachments handle Nothing = do
  let logh = hLogger handle
  Logger.logDebug logh "Message hasn't Attachments."
  return Nothing
updateAttachments handle (Just attachs) = do
  let logh = hLogger handle
  Logger.logDebug logh "Message has Attachments."
  Just <$> mapM (updateAttachment handle) attachs

updateAttachment :: Monad m => Handle m -> Attachment -> m Attachment
updateAttachment handle attach = do
  let attachType = attach_type attach
  case attachType of
    "doc" -> do
      docNew <- updateDoc handle $ fromJust $ attach_doc attach
      return attach { attach_doc = Just docNew }
    _ -> return attach

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

module Bot.Vk.Request.RequestsSpec where

import Prelude hiding (readFile)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Web.FormUrlEncoded as Url
import qualified Data.Text as T
import qualified Control.Monad.Catch as Exc
import Data.Text (Text)
import Text.Read (readMaybe)
import Control.Monad.Catch (MonadThrow)

import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Exception as E
import qualified Bot.Vk.Request.Data as RD
import Bot.Vk.Parser.Data (Attachment(..), Geo(..), Document(..),
                           Sticker(..), Audio(..), Video(..),
                           Photo(..), UserID, RepNum)
import Bot.Vk.Request.Data (SendMessage(..), VkRequest)
import Bot.Util (convert)

data Handle m = Handle {
    hLogger :: Logger.Handle m,
    configReq :: Settings.Config,
    hParser :: ParserSpec.Handle m,
    
    readFile :: FilePath -> m String,
    makeRequest :: VkRequest -> Text -> m B.ByteString,
    getUpdate :: Text -> Text -> Integer -> m B.ByteString
}

-- | Server query
createServerQuery :: (MonadThrow m, Monad m) => Handle m -> m Text
createServerQuery handle = do
  let logh = hLogger handle
      config = configReq handle
  case Settings.botGroupId config of
    Nothing -> do
      let msg = "'bot_group_id' is required for Api Vk"
      Logger.logError logh msg
      Exc.throwM $ E.ParseConfigError $ T.unpack msg
    Just groupId -> do
      let token = Settings.botToken config
          params = RD.getPollServer groupId token Settings.vkVersion
      Logger.logDebug logh "Server query string was created."
      return $ T.pack $ L8.unpack $ Url.urlEncodeAsFormStable params

getServer :: (MonadThrow m, Monad m) => Handle m -> m B.ByteString
getServer handle = do
  let logh = hLogger handle
  queryOptions <- createServerQuery handle
  Logger.logInfo logh "Get server parameters for requests."
  makeRequest handle RD.getLongPollServer queryOptions

-- | sendHelpMessage
createHelpMessage :: Monad m => Handle m -> UserID -> m Text
createHelpMessage handle userId = do
  let logh = hLogger handle
      config = configReq handle
      description = Settings.botDescription config
      token = Settings.botToken config
      message = (RD.defaultMessage userId token Settings.vkVersion) {
        sendMessag_message = description
      }
  Logger.logDebug logh "Help Message was created."
  return $ T.pack $ L8.unpack $ Url.urlEncodeAsFormStable message

sendHelpMessage :: Monad m => Handle m -> UserID -> m ()
sendHelpMessage handle userId = do
  let logh = hLogger handle
  queryOptions <- createHelpMessage handle userId
  _ <- makeRequest handle RD.sendMessage queryOptions
  Logger.logInfo logh $ "Help message was sended to chat with id: "
    <> convert userId

-- | send EchoMessage
geoToLatLong :: Maybe Geo -> [Maybe Double]
geoToLatLong Nothing = [Nothing, Nothing]
geoToLatLong (Just geo) = map 
  (\x -> readMaybe  x :: Maybe Double) 
  (words $ T.unpack $ geo_coordinates geo)

createEchoMessage :: Monad m => Handle m -> UserID -> Text ->
                     Maybe [Attachment] -> Maybe Geo -> m Text
createEchoMessage handle userId text atts geo = do
  let logh = hLogger handle
      config = configReq handle
      token = Settings.botToken config
      [lat, long] = geoToLatLong geo
      message = (RD.defaultMessage userId token Settings.vkVersion) {
        sendMessag_message = text,
        sendMessag_attachment = attachmentsToQuery atts,
        sendMessag_stickerId = returnStickerId atts,
        sendMessag_lat = lat,
        sendMessag_long = long
    }
  Logger.logDebug logh "Echo Message was created."
  return $ T.pack $ L8.unpack $ Url.urlEncodeAsFormStable message

sendEchoMessage :: Monad m => Handle m -> UserID -> Text ->
                   Maybe [Attachment] -> Maybe Geo -> m ()
sendEchoMessage handle userId text atts geo = do
  let logh = hLogger handle
  queryOptions <- createEchoMessage handle userId text atts geo
  _ <- makeRequest handle RD.sendMessage queryOptions
  Logger.logInfo logh $ "Echo message was sended to chat with id: "
    <> convert userId

sendNEchoMessage :: Monad m => Handle m -> UserID -> Text ->
                    Maybe [Attachment] -> Maybe Geo -> RepNum -> m ()
sendNEchoMessage handle _ _ _ _ 0 = do
  let logh = hLogger handle
  Logger.logInfo logh "Echo-Messages were sended."
sendNEchoMessage handle userId text atts geo n = do
  sendEchoMessage handle userId text atts geo
  sendNEchoMessage handle userId text atts geo (n-1)

-- | sendRepeatMessage
createRepeatMessage :: Monad m => Handle m -> UserID -> m Text
createRepeatMessage handle userId = do
  let logh = hLogger handle
      config = configReq handle
      token = Settings.botToken config
      question = Settings.botQuestion config
  keyboardF <- readFile handle "data/Vk/repeatButtons.txt"
  let message = (RD.defaultMessage userId token Settings.vkVersion) {
    sendMessag_message = question
  }
  Logger.logDebug logh "Repeat Message was created."
  return $ T.pack $ L8.unpack (Url.urlEncodeAsFormStable message)
    <> "&keyboard="
    <> keyboardF

sendRepeatMessage :: Monad m => Handle m -> UserID -> m ()
sendRepeatMessage handle userId = do
  let logh = hLogger handle
  queryOptions <- createRepeatMessage handle userId
  _ <- makeRequest handle RD.sendMessage queryOptions
  Logger.logInfo logh $ "Repeat message was sended to chat with id: "
    <> convert userId

attachmentsToQuery :: Maybe [Attachment] -> Maybe Text
attachmentsToQuery Nothing = Nothing
attachmentsToQuery (Just xs) = do
  let attsStrsL = filter (/= Nothing) (map attachmentToString xs)
  case attsStrsL of
    [] -> Nothing
    attsStrsML -> do
      attsStrs <- sequenceA attsStrsML
      Just $ T.intercalate "," attsStrs

attachmentToString :: Attachment -> Maybe Text
attachmentToString attach = case attach_type attach of 
    "photo" -> do
      ownerId <- photo_ownerId <$> attach_photo attach
      photoId <- photo_id <$> attach_photo attach
      key <- photo_accessKey <$> attach_photo attach
      return $ attach_type attach 
        <> convert ownerId 
        <> "_"
        <> convert photoId
        <> "_"
        <> key
    "video" -> do
      ownerId <- video_ownerId <$> attach_video attach
      videoId <- video_id <$> attach_video attach
      key <- video_accessKey <$> attach_video attach
      return $ attach_type attach 
        <> convert ownerId 
        <> "_"
        <> convert videoId
        <> "_"
        <> key
    "audio" -> do
      ownerId <- audio_ownerId <$> attach_audio attach
      audioId <- audio_id <$> attach_audio attach
      return $ attach_type attach 
        <> convert ownerId 
        <> "_"
        <> convert audioId
    "doc" -> do
      ownerId <- document_ownerId <$> attach_doc attach
      docId <- document_id <$> attach_doc attach
      return $ attach_type attach 
        <> convert ownerId 
        <> "_"
        <> convert docId
    _ -> Nothing

returnStickerId :: Maybe [Attachment] -> Maybe Integer
returnStickerId xsm = do
  xs <- xsm
  let awSticker = filter (\x -> attach_type x == "sticker") xs
  case awSticker of
    [x] -> sticker_id <$> attach_sticker x
    _ -> Nothing -- Sticker maybe only one in Message
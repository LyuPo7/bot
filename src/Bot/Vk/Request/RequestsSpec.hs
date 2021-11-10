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
import Bot.Vk.Parser.Objects.Synonyms (UserId, RepNum)
import Bot.Vk.Parser.Objects.Attachment (Attachment(..))
import Bot.Vk.Parser.Objects.Geo (Geo(..))
import qualified Bot.Vk.Parser.Objects.Geo as Geo
import qualified Bot.Vk.Parser.Objects.Document as Doc
import qualified Bot.Vk.Parser.Objects.Photo as Photo
import qualified Bot.Vk.Parser.Objects.Audio as Audio
import qualified Bot.Vk.Parser.Objects.Video as Video
import qualified Bot.Vk.Parser.Objects.Sticker as Sticker
import Bot.Vk.Request.Objects.VkRequest (VkRequest, sendMessage, getLongPollServer)
import Bot.Vk.Request.Objects.SendMessage (SendMessage(..), defaultMessage)
import Bot.Vk.Request.Objects.GetLongPollServer (getPollServer)
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
  let logH = hLogger handle
      config = configReq handle
  case Settings.botGroupId config of
    Nothing -> do
      let msg = "'bot_group_id' is required for Api Vk"
      Logger.logError logH msg
      Exc.throwM $ E.ParseConfigError $ T.unpack msg
    Just groupId -> do
      let token = Settings.botToken config
          params = getPollServer groupId token Settings.vkVersion
      Logger.logDebug logH "Server query string was created."
      return $ T.pack $ L8.unpack $ Url.urlEncodeAsFormStable params

getServer :: (MonadThrow m, Monad m) => Handle m -> m B.ByteString
getServer handle = do
  let logH = hLogger handle
  queryOptions <- createServerQuery handle
  Logger.logInfo logH "Get server parameters for requests."
  makeRequest handle getLongPollServer queryOptions

-- | sendHelpMessage
createHelpMessage :: Monad m => Handle m -> UserId -> m Text
createHelpMessage handle userId = do
  let logH = hLogger handle
      config = configReq handle
      description = Settings.botDescription config
      token = Settings.botToken config
      newMessage = (defaultMessage userId token Settings.vkVersion) {
        message = description
      }
  Logger.logDebug logH "Help Message was created."
  return $ T.pack $ L8.unpack $ Url.urlEncodeAsFormStable newMessage

sendHelpMessage :: Monad m => Handle m -> UserId -> m ()
sendHelpMessage handle userId = do
  let logH = hLogger handle
  queryOptions <- createHelpMessage handle userId
  _ <- makeRequest handle sendMessage queryOptions
  Logger.logInfo logH $ "Help message was sended to chat with id: "
    <> convert userId

-- | send EchoMessage
geoToLatLong :: Maybe Geo -> [Maybe Double]
geoToLatLong Nothing = [Nothing, Nothing]
geoToLatLong (Just geo) = map 
  (\x -> readMaybe  x :: Maybe Double) 
  (words $ T.unpack $ Geo.coordinates geo)

createEchoMessage :: Monad m => Handle m -> UserId -> Text ->
                     Maybe [Attachment] -> Maybe Geo -> m Text
createEchoMessage handle userId text atts geo = do
  let logH = hLogger handle
      config = configReq handle
      token = Settings.botToken config
      [coordLat, coordLong] = geoToLatLong geo
      newMessage = (defaultMessage userId token Settings.vkVersion) {
        message = text,
        attachment = attachmentsToQuery atts,
        sticker_id = returnStickerId atts,
        lat = coordLat,
        long = coordLong
    }
  Logger.logDebug logH "Echo Message was created."
  return $ T.pack $ L8.unpack $ Url.urlEncodeAsFormStable newMessage

sendEchoMessage :: Monad m => Handle m -> UserId -> Text ->
                   Maybe [Attachment] -> Maybe Geo -> m ()
sendEchoMessage handle userId text atts geo = do
  let logH = hLogger handle
  queryOptions <- createEchoMessage handle userId text atts geo
  _ <- makeRequest handle sendMessage queryOptions
  Logger.logInfo logH $ "Echo message was sended to chat with id: "
    <> convert userId

sendNEchoMessage :: Monad m => Handle m -> UserId -> Text ->
                    Maybe [Attachment] -> Maybe Geo -> RepNum -> m ()
sendNEchoMessage handle _ _ _ _ 0 = do
  let logH = hLogger handle
  Logger.logInfo logH "Echo-Messages were sended."
sendNEchoMessage handle userId text atts geo n = do
  sendEchoMessage handle userId text atts geo
  sendNEchoMessage handle userId text atts geo (n-1)

-- | sendRepeatMessage
createRepeatMessage :: Monad m => Handle m -> UserId -> m Text
createRepeatMessage handle userId = do
  let logH = hLogger handle
      config = configReq handle
      token = Settings.botToken config
      question = Settings.botQuestion config
  keyboardF <- readFile handle "data/Vk/repeatButtons.txt"
  let newMessage = (defaultMessage userId token Settings.vkVersion) {
    message = question
  }
  Logger.logDebug logH "Repeat Message was created."
  return $ T.pack $ L8.unpack (Url.urlEncodeAsFormStable newMessage)
    <> "&keyboard="
    <> keyboardF

sendRepeatMessage :: Monad m => Handle m -> UserId -> m ()
sendRepeatMessage handle userId = do
  let logH = hLogger handle
  queryOptions <- createRepeatMessage handle userId
  _ <- makeRequest handle sendMessage queryOptions
  Logger.logInfo logH $ "Repeat message was sended to chat with id: "
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
attachmentToString attach = case attach of 
  AttachPhoto photo -> do
    let ownerId = Photo.owner_id photo
        photoId = Photo.id photo
        key = Photo.access_key photo
    return $ "photo" 
      <> convert ownerId 
      <> "_"
      <> convert photoId
      <> "_"
      <> key
  AttachVideo video -> do
    let ownerId = Video.owner_id video
        videoId = Video.id video
        key = Video.access_key video
    return $ "video" 
      <> convert ownerId 
      <> "_"
      <> convert videoId
      <> "_"
      <> key
  AttachAudio audio -> do
    let ownerId = Audio.owner_id audio
        audioId = Audio.id audio
    return $ "audio" 
      <> convert ownerId 
      <> "_"
      <> convert audioId
  AttachDoc doc -> do
    let ownerId = Doc.owner_id doc
        docId = Doc.id doc
    return $ "doc"
      <> convert ownerId 
      <> "_"
      <> convert docId
  _ -> Nothing

returnStickerId :: Maybe [Attachment] -> Maybe Integer
returnStickerId attachsM = do
  attachs <- attachsM
  let awSticker = [ x | x@AttachSticker {} <- attachs ]
  case awSticker of
    [AttachSticker sticker] -> Just $ Sticker.id sticker
    _ -> Nothing -- Sticker maybe only one in Message
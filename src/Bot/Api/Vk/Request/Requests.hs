module Bot.Api.Vk.Request.Requests where

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (newEitherT, runEitherT)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Convertible.Base (convert)
import Data.List (union, (\\))
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath.Posix ((</>))
import Text.Read (readMaybe)
import qualified Web.FormUrlEncoded as Url
import Prelude hiding (readFile)

import qualified Bot.Api.Vk.Objects.Attachment as VkAttach
import qualified Bot.Api.Vk.Objects.Audio as VkAudio
import qualified Bot.Api.Vk.Objects.Document as VkDoc
import qualified Bot.Api.Vk.Objects.Geo as VkGeo
import qualified Bot.Api.Vk.Objects.GetLongPollServer as VkGetLongPollServer
import qualified Bot.Api.Vk.Objects.GetUploadLink as VkGetUpLink
import qualified Bot.Api.Vk.Objects.Message as VkMessage
import qualified Bot.Api.Vk.Objects.Method as VkMethod
import qualified Bot.Api.Vk.Objects.Photo as VkPhoto
import qualified Bot.Api.Vk.Objects.RequestOptions as VkReqOptions
import qualified Bot.Api.Vk.Objects.SaveDoc as VkSaveDoc
import qualified Bot.Api.Vk.Objects.SendMessage as VkSendMessage
import qualified Bot.Api.Vk.Objects.Server as VkServer
import qualified Bot.Api.Vk.Objects.Sticker as VkSticker
import qualified Bot.Api.Vk.Objects.UploadFileResponse as VkUpFileResp
import qualified Bot.Api.Vk.Objects.UploadObject as VkUpObj
import qualified Bot.Api.Vk.Objects.UploadObjectResponse as VkUpObjResp
import qualified Bot.Api.Vk.Objects.UploadUrl as VkUpUrl
import qualified Bot.Api.Vk.Objects.UploadUrlResponse as VkUpUrlResp
import qualified Bot.Api.Vk.Objects.Video as VkVideo
import qualified Bot.DB.DB as BotDB
import qualified Bot.Exception as E
import qualified Bot.Logger.Logger as Logger
import qualified Bot.Objects.Button as BotButton
import qualified Bot.Objects.Document as BotDoc
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Objects.RequestPair as BotReqPair
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Settings as Settings
import qualified Bot.System.System as BotSystem
import qualified Bot.Util as BotUtil

setGetServer ::
  (MonadThrow m, Monad m) =>
  BotDB.Handle m ->
  m (Maybe BotReqPair.ReqPair)
setGetServer handle = do
  let logH = BotDB.hLogger handle
      config = BotDB.cDb handle
  case Settings.botGroupId config of
    Nothing -> do
      let msg = "'bot_group_id' is required for Api Vk"
      Logger.logError logH msg
      throwM $ E.ParseConfigError $ T.unpack msg
    Just groupId -> do
      let token = Settings.botToken config
          params =
            VkGetLongPollServer.getPollServer
              groupId
              token
              Settings.vkVersion
          reqOptions =
            VkReqOptions.RequestOptions $
              T.pack $ L8.unpack $ Url.urlEncodeAsFormStable params
          apiMethod = VkMethod.getLongPollServer
      Logger.logDebug logH "Server query string was created."
      return $ Just $ BotReqPair.VkReqPair (apiMethod, reqOptions)

setUploadedServer ::
  Monad m =>
  BotDB.Handle m ->
  BotDoc.Document ->
  m (Maybe BotReqPair.ReqPair)
setUploadedServer handle botDoc = do
  let config = BotDB.cDb handle
      token = Settings.botToken config
      doc = BotDoc.vkDoc botDoc
      userId = VkDoc.owner_id doc
      query =
        VkGetUpLink.getLink
          userId
          (BotSynonyms.FileType "doc")
          Settings.vkVersion
          token
      vkReqOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable query
      reqOptions = VkReqOptions.RequestOptions vkReqOptions
      apiMethod = VkMethod.getMessagesUploadServer
  return $ Just $ BotReqPair.VkReqPair (apiMethod, reqOptions)

setUploadedDoc ::
  Monad m =>
  BotDB.Handle m ->
  Text ->
  m (Maybe BotReqPair.ReqPair)
setUploadedDoc handle file = do
  let config = BotDB.cDb handle
      token = Settings.botToken config
      link =
        VkSaveDoc.saveNewDoc
          (BotSynonyms.FilePathT file)
          token
          Settings.vkVersion
      vkReqOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable link
      reqOptions = VkReqOptions.RequestOptions vkReqOptions
      apiMethod = VkMethod.saveDoc
  return $ Just $ BotReqPair.VkReqPair (apiMethod, reqOptions)

setEchoMessage ::
  (MonadThrow m, Monad m) =>
  BotDB.Handle m ->
  BotMessage.Message ->
  m BotReqPair.ReqPair
setEchoMessage handle (BotMessage.VkMessage message) = do
  let config = BotDB.cDb handle
      token = Settings.botToken config
      userId = VkMessage.user_id message
      messageText = VkMessage.body message
      messageAttachs = VkMessage.attachments message
      messageGeo = VkMessage.geo message
      [coordLat, coordLong] = geoToLatLong messageGeo
  let newMessage =
        ( VkSendMessage.defaultMessage
            userId
            token
            Settings.vkVersion
        )
          { VkSendMessage.message = messageText,
            VkSendMessage.attachment = attachmentsToQuery messageAttachs,
            VkSendMessage.sticker_id = returnStickerId messageAttachs,
            VkSendMessage.lat = coordLat,
            VkSendMessage.long = coordLong
          }
      vkReqOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable newMessage
      reqOptions = VkReqOptions.RequestOptions vkReqOptions
      apiMethod = VkMethod.sendMessage
  return $ BotReqPair.VkReqPair (apiMethod, reqOptions)
setEchoMessage _ botMessage = do
  throwM $ E.ApiObjectError $ show botMessage

setHelpMessage ::
  (MonadThrow m, Monad m) =>
  BotDB.Handle m ->
  BotMessage.Message ->
  Text ->
  m (Maybe BotReqPair.ReqPair)
setHelpMessage handle (BotMessage.VkMessage message) description = do
  let config = BotDB.cDb handle
      token = Settings.botToken config
      userId = VkMessage.user_id message
      newMessage =
        ( VkSendMessage.defaultMessage userId token Settings.vkVersion
        )
          { VkSendMessage.message = description
          }
      vkReqOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable newMessage
      reqOptions = VkReqOptions.RequestOptions vkReqOptions
      apiMethod = VkMethod.sendMessage
  return $ Just $ BotReqPair.VkReqPair (apiMethod, reqOptions)
setHelpMessage _ botMessage _ = do
  throwM $ E.ApiObjectError $ show botMessage

setKeyboardMessage ::
  (MonadThrow m, Monad m) =>
  BotDB.Handle m ->
  BotMessage.Message ->
  [BotButton.Button] ->
  Text ->
  m BotReqPair.ReqPair
setKeyboardMessage handle (BotMessage.VkMessage message) _ question = do
  let systemH = BotDB.hSystem handle
      config = BotDB.cDb handle
      token = Settings.botToken config
      userId = VkMessage.user_id message
  keyboard <- BotSystem.readFile systemH "data/Vk/repeatButtons.txt"
  let newMessage =
        ( VkSendMessage.defaultMessage userId token Settings.vkVersion
        )
          { VkSendMessage.message = question
          }
      vkReqOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable newMessage
      vkReqOptionsWKeyboard = vkReqOptions <> "&keyboard=" <> T.pack keyboard
      reqOptions = VkReqOptions.RequestOptions vkReqOptionsWKeyboard
      apiMethod = VkMethod.sendMessage
  return $ BotReqPair.VkReqPair (apiMethod, reqOptions)
setKeyboardMessage _ botMessage _ _ = do
  throwM $ E.ApiObjectError $ show botMessage

setGetUpdate ::
  (MonadThrow m, Monad m) =>
  BotDB.Handle m ->
  BotUpdate.Update ->
  m BotReqPair.ReqPair
setGetUpdate _ (BotUpdate.VkUpdate server) = do
  let serverLink = VkServer.server server
      serverKey = VkServer.key server
      tsCurrent = VkServer.ts server
      vkReqOptions =
        serverLink
          <> "?act=a_check&key="
          <> serverKey
          <> "&ts="
          <> BotUtil.convertValue tsCurrent
          <> "&wait="
          <> BotUtil.convertValue Settings.timeout
      reqOptions = VkReqOptions.RequestOptions vkReqOptions
      apiMethod = VkMethod.getUpdate
  return $ BotReqPair.VkReqPair (apiMethod, reqOptions)
setGetUpdate _ botUpdate = do
  throwM $ E.ApiObjectError $ show botUpdate

createRequest ::
  (Monad m, MonadThrow m) =>
  BotDB.Handle m ->
  BotReqPair.ReqPair ->
  m (Text, B.ByteString)
createRequest _ (BotReqPair.VkReqPair (apiMethod, options)) = do
  let methodApi = VkMethod.getMethod apiMethod
      optionsApi = VkReqOptions.reqOption options
      api = T.intercalate "?" (filter (not . T.null) [methodApi, optionsApi])
  return (api, "")
createRequest _ _ = do
  throwM E.ApiMethodError

downloadDoc ::
  (Monad m, MonadThrow m) =>
  BotDB.Handle m ->
  BotDoc.Document ->
  B.ByteString ->
  m (Maybe Text)
downloadDoc handle botDoc serverUp = do
  let systemH = BotDB.hSystem handle
      link = VkDoc.url doc
      docTitle = VkDoc.title doc
      doc = BotDoc.vkDoc botDoc
  tempDir <- BotSystem.getTemporaryDirectory systemH
  let fileName = tempDir </> T.unpack docTitle
  BotSystem.downloadFile systemH link fileName
  fileRespE <- runEitherT $ do
    urlResp <- newEitherT $ BotParser.parseData handle serverUp
    let docUrl =
          maybe
            (BotSynonyms.Url "")
            VkUpUrl.upload_url
            (VkUpUrlResp.response urlResp)
    fileUp <- lift $ BotSystem.uploadFile systemH docUrl fileName
    newEitherT $ BotParser.parseData handle fileUp
  case fileRespE of
    Left msg -> throwM $ E.ParseRequestError $ T.unpack msg
    Right fileResp -> return $ VkUpFileResp.file fileResp

changeDoc ::
  (Monad m, MonadThrow m) =>
  BotDB.Handle m ->
  BotDoc.Document ->
  B.ByteString ->
  m BotDoc.Document
changeDoc handle botDoc@(BotDoc.VkDoc doc) objUp = do
  let logH = BotDB.hLogger handle
  objE <- BotParser.parseData handle objUp
  case objE of
    Left msg -> throwM $ E.ParseRequestError $ T.unpack msg
    Right obj -> do
      case VkUpObjResp.response obj of
        [x] -> do
          Logger.logInfo logH "Doc changed"
          let (BotSynonyms.ObjectId objId) = VkUpObj.id x
          return $
            BotDoc.VkDoc
              doc
                { VkDoc.id = BotSynonyms.DocId objId,
                  VkDoc.owner_id = VkUpObj.owner_id x,
                  VkDoc.url = VkUpObj.url x
                }
        _ -> do
          Logger.logWarning logH "No uploaded objects"
          return botDoc

extractDoc ::
  (MonadThrow m, Monad m) =>
  BotDB.Handle m ->
  BotMessage.Message ->
  m (Maybe [BotDoc.Document])
extractDoc _ (BotMessage.VkMessage message) = do
  case VkMessage.attachments message of
    Nothing -> return Nothing
    Just attachs -> do
      let vkDocs = [doc | (VkAttach.AttachDoc doc) <- attachs]
      return $ Just $ map BotDoc.VkDoc vkDocs
extractDoc _ botMessage = do
  throwM $ E.ApiObjectError $ show botMessage

changeMessage ::
  (MonadThrow m, Monad m) =>
  BotDB.Handle m ->
  BotMessage.Message ->
  [BotDoc.Document] ->
  m BotMessage.Message
changeMessage _ botMessage@(BotMessage.VkMessage message) botDocs = do
  case map BotDoc.vkDoc botDocs of
    [] -> return botMessage
    docs -> do
      case VkMessage.attachments message of
        Nothing -> return botMessage
        Just attachs -> do
          let attachDocs = [x | x@VkAttach.AttachDoc {} <- attachs]
              attachNoDocs = attachs \\ attachDocs
              newAttachs = attachNoDocs `union` map VkAttach.AttachDoc docs
              newMessage = message {VkMessage.attachments = Just newAttachs}
          return $ BotMessage.VkMessage newMessage
changeMessage _ botMessage _ = do
  throwM $ E.ApiObjectError $ show botMessage

attachmentsToQuery ::
  Maybe [VkAttach.Attachment] ->
  Maybe Text
attachmentsToQuery Nothing = Nothing
attachmentsToQuery (Just xs) = do
  let attsStrsL = filter (/= Nothing) (map attachmentToString xs)
  case attsStrsL of
    [] -> Nothing
    attsStrsML -> do
      attsStrs <- sequenceA attsStrsML
      Just $ T.intercalate "," attsStrs

attachmentToString ::
  VkAttach.Attachment ->
  Maybe Text
attachmentToString attach = case attach of
  VkAttach.AttachPhoto photo -> do
    let ownerId = VkPhoto.owner_id photo
        photoId = VkPhoto.id photo
        key = VkPhoto.access_key photo
    return $
      "photo"
        <> BotUtil.convertValue ownerId
        <> "_"
        <> BotUtil.convertValue photoId
        <> "_"
        <> convert key
  VkAttach.AttachVideo video -> do
    let ownerId = VkVideo.owner_id video
        videoId = VkVideo.id video
        key = VkVideo.access_key video
    return $
      "video"
        <> BotUtil.convertValue ownerId
        <> "_"
        <> BotUtil.convertValue videoId
        <> "_"
        <> convert key
  VkAttach.AttachAudio audio -> do
    let ownerId = VkAudio.owner_id audio
        audioId = VkAudio.id audio
    return $
      "audio"
        <> BotUtil.convertValue ownerId
        <> "_"
        <> BotUtil.convertValue audioId
  VkAttach.AttachDoc doc -> do
    let ownerId = VkDoc.owner_id doc
        docId = VkDoc.id doc
    return $
      "doc"
        <> BotUtil.convertValue ownerId
        <> "_"
        <> BotUtil.convertValue docId
  _ -> Nothing

returnStickerId ::
  Maybe [VkAttach.Attachment] ->
  Maybe BotSynonyms.StickerId
returnStickerId attachsM = do
  attachs <- attachsM
  let awSticker = [x | x@VkAttach.AttachSticker {} <- attachs]
  case awSticker of
    [VkAttach.AttachSticker sticker] -> Just $ VkSticker.id sticker
    _ -> Nothing -- Sticker maybe only one in Message

geoToLatLong ::
  Maybe VkGeo.Geo ->
  [Maybe Double]
geoToLatLong Nothing = [Nothing, Nothing]
geoToLatLong (Just geo) =
  map
    (\x -> readMaybe x :: Maybe Double)
    (words $ T.unpack $ VkGeo.coordinates geo)

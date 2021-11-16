module Bot.Api.Vk.Request.Requests where

import Prelude hiding (readFile)
import Data.List (union, (\\))
import System.FilePath.Posix ((</>))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Web.FormUrlEncoded as Url
import qualified Data.Text as T
import qualified Control.Exception as Exc
import qualified Control.Monad.Catch as Catch
import Data.Text (Text)
import Text.Read (readMaybe)
import Control.Monad.Catch (MonadThrow)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode, statusCode)
import Network.HTTP.Client (Request(..),
                            newManager, parseRequest, httpLbs,
                            responseStatus, responseBody)

import qualified Bot.Settings as Settings
import qualified Bot.Exception as E
import qualified Bot.Logger.Logger as Logger
import qualified Bot.DB.DBQ as BotDBQ
import qualified Bot.System.System as BotSystem
import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Request.Request as BotReq
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Method as BotMethod
import qualified Bot.Objects.RequestOptions as BotReqOptions
import qualified Bot.Objects.Button as BotButton
import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Objects.Document as BotDoc
import qualified Bot.Api.Vk.Objects.Message as VkMessage
import qualified Bot.Api.Vk.Objects.Attachment as VkAttach
import qualified Bot.Api.Vk.Objects.Geo as VkGeo
import qualified Bot.Api.Vk.Objects.Document as VkDoc
import qualified Bot.Api.Vk.Objects.Photo as VkPhoto
import qualified Bot.Api.Vk.Objects.Audio as VkAudio
import qualified Bot.Api.Vk.Objects.Video as VkVideo
import qualified Bot.Api.Vk.Objects.Sticker as VkSticker
import qualified Bot.Api.Vk.Objects.Method as VkMethod
import qualified Bot.Api.Vk.Objects.RequestOptions as VkReqOptions
import qualified Bot.Api.Vk.Objects.SendMessage as VkSendMessage
import qualified Bot.Api.Vk.Objects.Server as VkServer
import qualified Bot.Api.Vk.Objects.GetLongPollServer as VkGetLongPollServer
import qualified Bot.Api.Vk.Objects.GetUploadLink as VkGetUpLink
import qualified Bot.Api.Vk.Objects.UploadObject as VkUpObj
import qualified Bot.Api.Vk.Objects.UploadUrl as VkUpUrl
import qualified Bot.Api.Vk.Objects.UploadObjectResponse as VkUpObjResp
import qualified Bot.Api.Vk.Objects.UploadFileResponse as VkUpFileResp
import qualified Bot.Api.Vk.Objects.UploadUrlResponse as VkUpUrlResp
import qualified Bot.Api.Vk.Objects.SaveDoc as VkSaveDoc
import qualified Bot.Api.Vk.Parser.Parser as Parser
import qualified Bot.Util as BotUtil

withHandleIO :: Logger.Handle IO -> BotDBQ.Handle IO -> BotParser.Handle IO ->
                Settings.Config -> (BotReq.Handle IO -> IO a) -> IO a
withHandleIO logger dbH parserH config f = do
  let handle = BotReq.Handle {
    BotReq.hLogger = logger,
    BotReq.hDb = dbH,
    BotReq.hParser = parserH,
    BotReq.cReq = config,

    BotReq.makeRequest = makeRequest parserH,
    BotReq.setUploadedServer = setUploadedServer parserH,
    BotReq.setUploadedDoc = setUploadedDoc parserH,
    BotReq.setGetServer = setGetServer parserH,
    BotReq.setGetUpdate = setGetUpdate parserH,
    BotReq.setEchoMessage = setEchoMessage parserH,
    BotReq.setHelpMessage = setHelpMessage parserH,
    BotReq.setStartMessage = \_ _ -> return Nothing,
    BotReq.setKeyboardMessage = setKeyboardMessage parserH,
    BotReq.setCommands = return Nothing,

    BotReq.downloadDoc = downloadDoc parserH,
    BotReq.extractDoc = extractDoc parserH,
    BotReq.changeMessage = changeMessage parserH,
    BotReq.changeDoc = changeDoc parserH
  }
  f handle

setGetServer :: (MonadThrow m, Monad m) => BotParser.Handle m ->
                 m (Maybe (BotMethod.Method, BotReqOptions.RequestOptions))
setGetServer handle = do
  let logH = BotParser.hLogger handle
      config = BotParser.cParser handle
  case Settings.botGroupId config of
    Nothing -> do
      let msg = "'bot_group_id' is required for Api Vk"
      Logger.logError logH msg
      Catch.throwM $ E.ParseConfigError $ T.unpack msg
    Just groupId -> do
      let token = Settings.botToken config
          params = VkGetLongPollServer.getPollServer
            groupId token Settings.vkVersion
          reqOptions = BotReqOptions.VkReqOptions $
                       VkReqOptions.RequestOptions $
                       T.pack $ L8.unpack $ Url.urlEncodeAsFormStable params
          apiMethod = BotMethod.VkMethod VkMethod.getLongPollServer
      Logger.logDebug logH "Server query string was created."
      return $ Just (apiMethod, reqOptions)

setUploadedServer :: Monad m => BotParser.Handle m -> BotDoc.Document ->
                  m (Maybe (BotMethod.Method, BotReqOptions.RequestOptions))
setUploadedServer handle botDoc = do
  let config = BotParser.cParser handle
      token = Settings.botToken config
      doc = BotDoc.vkDoc botDoc
      userId = VkDoc.owner_id doc
      query = VkGetUpLink.getLink userId "doc" Settings.vkVersion token
      vkReqOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable query
      reqOptions = BotReqOptions.VkReqOptions $
                   VkReqOptions.RequestOptions vkReqOptions
      apiMethod = BotMethod.VkMethod VkMethod.getMessagesUploadServer
  return $ Just (apiMethod, reqOptions)

setUploadedDoc :: Monad m => BotParser.Handle m -> Text ->
                  m (Maybe (BotMethod.Method, BotReqOptions.RequestOptions))
setUploadedDoc handle file = do
  let config = BotParser.cParser handle
      token = Settings.botToken config
      link = VkSaveDoc.saveNewDoc file token Settings.vkVersion
      vkReqOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable link
      reqOptions = BotReqOptions.VkReqOptions $
                   VkReqOptions.RequestOptions vkReqOptions
      apiMethod = BotMethod.VkMethod VkMethod.saveDoc
  return $ Just (apiMethod, reqOptions)

setEchoMessage :: Monad m => BotParser.Handle m -> BotMessage.Message ->
                  m (BotMethod.Method, BotReqOptions.RequestOptions)
setEchoMessage handle botMessage = do
  let config = BotParser.cParser handle
      token = Settings.botToken config
      message = BotMessage.vkMessage botMessage
      userId = VkMessage.user_id message
      messageText = VkMessage.body message
      messageAttachs = VkMessage.attachments message
      messageGeo = VkMessage.geo message
      [coordLat, coordLong] = geoToLatLong messageGeo
  let newMessage = (VkSendMessage.defaultMessage 
        userId token Settings.vkVersion) {
          VkSendMessage.message = messageText,
          VkSendMessage.attachment = attachmentsToQuery messageAttachs,
          VkSendMessage.sticker_id = returnStickerId messageAttachs,
          VkSendMessage.lat = coordLat,
          VkSendMessage.long = coordLong
        }
      vkReqOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable newMessage
      reqOptions = BotReqOptions.VkReqOptions $
                   VkReqOptions.RequestOptions vkReqOptions
      apiMethod = BotMethod.VkMethod VkMethod.sendMessage
  return (apiMethod, reqOptions)

setHelpMessage :: Monad m => BotParser.Handle m -> BotMessage.Message ->
                  BotSynonyms.Description ->
                  m (BotMethod.Method, BotReqOptions.RequestOptions)
setHelpMessage handle botMessage description = do
  let config = BotParser.cParser handle
      token = Settings.botToken config
      message = BotMessage.vkMessage botMessage
      userId = VkMessage.user_id message
      newMessage = (
        VkSendMessage.defaultMessage userId token Settings.vkVersion) {
          VkSendMessage.message = description
        }
      vkReqOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable newMessage
      reqOptions = BotReqOptions.VkReqOptions $
                   VkReqOptions.RequestOptions vkReqOptions
      apiMethod = BotMethod.VkMethod VkMethod.sendMessage
  return (apiMethod, reqOptions)

setKeyboardMessage :: Monad m => BotParser.Handle m ->
                      BotMessage.Message ->
                     [BotButton.Button] ->
                      Text ->
                      m (BotMethod.Method, BotReqOptions.RequestOptions)
setKeyboardMessage handle botMessage _ question = do
  let systemH = BotParser.hSystem handle
      config = BotParser.cParser handle
      token = Settings.botToken config
      message = BotMessage.vkMessage botMessage
      userId = VkMessage.user_id message
  keyboard <- BotSystem.readFile systemH "data/Vk/repeatButtons.txt"
  let newMessage = (
        VkSendMessage.defaultMessage userId token Settings.vkVersion) {
          VkSendMessage.message = question
        }
      vkReqOptions = T.pack $ L8.unpack $ Url.urlEncodeAsFormStable newMessage
      vkReqOptionsWKeyboard = vkReqOptions <> "&keyboard=" <> T.pack keyboard
      reqOptions = BotReqOptions.VkReqOptions $
                   VkReqOptions.RequestOptions vkReqOptionsWKeyboard
      apiMethod = BotMethod.VkMethod VkMethod.sendMessage
  return (apiMethod, reqOptions)

setGetUpdate :: Monad m => BotParser.Handle m -> BotUpdate.Update ->
                m (BotMethod.Method, BotReqOptions.RequestOptions)
setGetUpdate _ botUpdate = do
  let server = BotUpdate.vkUpdate botUpdate
      serverLink = VkServer.server server
      serverKey = VkServer.key server
      tsCurrent = VkServer.ts server
      vkReqOptions = serverLink
                  <> "?act=a_check&key="
                  <> serverKey
                  <> "&ts="
                  <> BotUtil.convert tsCurrent
                  <> "&wait="
                  <> BotUtil.convert Settings.timeout
      reqOptions = BotReqOptions.VkReqOptions $
                   VkReqOptions.RequestOptions vkReqOptions
      apiMethod = BotMethod.VkMethod VkMethod.getUpdate
  return (apiMethod, reqOptions)

makeRequest :: BotParser.Handle IO -> BotMethod.Method ->
               BotReqOptions.RequestOptions -> IO B.ByteString
makeRequest handle botMethod botOptions = do
  let logH = BotParser.hLogger handle
      apiMethod = BotMethod.vkMethod botMethod
      options = BotReqOptions.vkReqOptions botOptions
      methodApi = VkMethod.getMethod apiMethod
      optionsApi = VkReqOptions.reqOption options
      apiOpt = T.intercalate "?" (filter (not . T.null) [methodApi, optionsApi])
  manager <- newManager tlsManagerSettings
  Logger.logDebug logH $ "Request request: " <> apiOpt
  initialRequest <- parseRequest $ T.unpack apiOpt
  let request = initialRequest {
    method = "POST",
    requestHeaders = [ 
        ("Content-Type", "application/json; charset=utf-8")
    ]
  }
  response <- httpLbs request manager
  let codeResp = statusCode $ responseStatus response
  if codeResp == 200
    then do
      Logger.logDebug logH "Successful request to api."
      return $ responseBody response
    else do
      Logger.logDebug logH $ "Unsuccessful request to api with code: "
       <> BotUtil.convert codeResp
      Exc.throwIO $ E.ConnectionError codeResp

downloadDoc :: Monad m => BotParser.Handle m ->
               BotDoc.Document -> B.ByteString -> m (Maybe Text)
downloadDoc handle botDoc serverUp = do
  let systemH = BotParser.hSystem handle
      link = VkDoc.url doc
      docTitle = VkDoc.title doc
      doc = BotDoc.vkDoc botDoc
  tempDir <- BotSystem.getTemporaryDirectory systemH
  let fileName = tempDir </> T.unpack docTitle
  BotSystem.downloadFile systemH link fileName
  urlResp <- Parser.parseUploadUrl handle serverUp
  let docUrl = maybe (T.pack "") VkUpUrl.upload_url (VkUpUrlResp.response urlResp)
  fileUp <- BotSystem.uploadFile systemH docUrl fileName
  fileResp <- Parser.parseUploadFile handle fileUp
  return $ VkUpFileResp.file fileResp

changeDoc :: Monad m => BotParser.Handle m ->
             BotDoc.Document -> B.ByteString -> m BotDoc.Document
changeDoc handle botDoc@(BotDoc.VkDoc doc) objUp = do
  let logH = BotParser.hLogger handle
  obj <- Parser.parseUploadObject handle objUp
  case VkUpObjResp.response obj of
    [x] -> do
      Logger.logInfo logH "Doc changed"
      return $ BotDoc.VkDoc doc {
        VkDoc.id = VkUpObj.id x,
        VkDoc.owner_id = VkUpObj.owner_id x,
        VkDoc.url = VkUpObj.url x
      }
    _ -> do
      Logger.logWarning logH "No uploaded objects"
      return botDoc

extractDoc :: Monad m => BotParser.Handle m ->
              BotMessage.Message -> m (Maybe [BotDoc.Document])
extractDoc _ botMessage = do
  let message = BotMessage.vkMessage botMessage
  case VkMessage.attachments message of
    Nothing -> return Nothing
    Just attachs -> do
      let vkDocs = [ doc | (VkAttach.AttachDoc doc) <- attachs ]
      return $ Just $ map BotDoc.VkDoc vkDocs

changeMessage :: Monad m => BotParser.Handle m -> 
                 BotMessage.Message -> [BotDoc.Document] -> m BotMessage.Message
changeMessage _ botMessage botDocs = do
  let message = BotMessage.vkMessage botMessage
  case map BotDoc.vkDoc botDocs of
    [] -> return botMessage
    docs -> do
      case VkMessage.attachments message of
        Nothing -> return botMessage
        Just attachs -> do
          let attachDocs = [ x | x@VkAttach.AttachDoc {} <- attachs ]
              attachNoDocs = attachs \\ attachDocs
              newAttachs = attachNoDocs `union` map VkAttach.AttachDoc docs
              newMessage = message {VkMessage.attachments = Just newAttachs}
          return $ BotMessage.VkMessage newMessage

attachmentsToQuery :: Maybe [VkAttach.Attachment] -> Maybe Text
attachmentsToQuery Nothing = Nothing
attachmentsToQuery (Just xs) = do
  let attsStrsL = filter (/= Nothing) (map attachmentToString xs)
  case attsStrsL of
    [] -> Nothing
    attsStrsML -> do
      attsStrs <- sequenceA attsStrsML
      Just $ T.intercalate "," attsStrs

attachmentToString :: VkAttach.Attachment -> Maybe Text
attachmentToString attach = case attach of 
  VkAttach.AttachPhoto photo -> do
    let ownerId = VkPhoto.owner_id photo
        photoId = VkPhoto.id photo
        key = VkPhoto.access_key photo
    return $ "photo" 
      <> BotUtil.convert ownerId 
      <> "_"
      <> BotUtil.convert photoId
      <> "_"
      <> key
  VkAttach.AttachVideo video -> do
    let ownerId = VkVideo.owner_id video
        videoId = VkVideo.id video
        key = VkVideo.access_key video
    return $ "video" 
      <> BotUtil.convert ownerId 
      <> "_"
      <> BotUtil.convert videoId
      <> "_"
      <> key
  VkAttach.AttachAudio audio -> do
    let ownerId = VkAudio.owner_id audio
        audioId = VkAudio.id audio
    return $ "audio" 
      <> BotUtil.convert ownerId 
      <> "_"
      <> BotUtil.convert audioId
  VkAttach.AttachDoc doc -> do
    let ownerId = VkDoc.owner_id doc
        docId = VkDoc.id doc
    return $ "doc"
      <> BotUtil.convert ownerId 
      <> "_"
      <> BotUtil.convert docId
  _ -> Nothing

returnStickerId :: Maybe [VkAttach.Attachment] -> Maybe BotSynonyms.StickerId
returnStickerId attachsM = do
  attachs <- attachsM
  let awSticker = [ x | x@VkAttach.AttachSticker {} <- attachs ]
  case awSticker of
    [VkAttach.AttachSticker sticker] -> Just $ VkSticker.id sticker
    _ -> Nothing -- Sticker maybe only one in Message

geoToLatLong :: Maybe VkGeo.Geo -> [Maybe Double]
geoToLatLong Nothing = [Nothing, Nothing]
geoToLatLong (Just geo) = map 
  (\x -> readMaybe  x :: Maybe Double) 
  (words $ T.unpack $ VkGeo.coordinates geo)
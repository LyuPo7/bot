module Bot.Vk.RunSpec where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Control.Monad.Trans.Either as EiT
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Text (Text)
import Control.Monad.Catch (MonadThrow, throwM)

import qualified Bot.Exception as E
import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.DB.DBSpec as DB
import qualified Bot.Vk.Request.RequestsSpec as Req
import qualified Bot.Vk.Request.AttachSpec as Attach
import qualified Bot.Vk.Parser.ParserSpec as Parser
import Bot.Vk.Parser.Objects.Message (Message(..)) 
import Bot.Vk.Parser.Objects.Server (Server(..)) 
import Bot.Vk.Parser.Objects.UpdateData (UpdateData(..))
import Bot.Vk.Parser.Objects.Attachment (Attachment(..))
import Bot.Vk.Parser.Objects.Geo (Geo(..))
import Bot.Vk.Parser.Objects.UploadObjectResponse (UploadObjectResponse(..))
import Bot.Vk.Parser.Objects.UploadFileResponse (UploadFileResponse(..))
import Bot.Vk.Parser.Objects.UploadUrlResponse (UploadUrlResponse(..))
import Bot.Vk.Parser.Objects.PollResponse (PollResponse(..))

import qualified Bot.Vk.Parser.Objects.Update as Update
import qualified Bot.Vk.Parser.Objects.Message as Message
import qualified Bot.Vk.Parser.Objects.Server as Server
import qualified Bot.Vk.Parser.Objects.PollResponse as PollResp
import Bot.Vk.Parser.Objects.Synonyms (RepNum, Mode, UserId, UpdateId)
import Bot.Util (convert, readEitherMa)

data Handle m = Handle {
    hLogger :: Logger.Handle m,
    cRun :: Settings.Config,
    hDb :: DB.Handle m,
    hReq :: Req.Handle m,
    hParser :: Parser.Handle m,
    hAttach :: Attach.Handle m,
    
    parsePollResponse :: L8.ByteString -> m (Either Text PollResponse),
    parseUpdateData :: L8.ByteString -> m UpdateData,
    parseUploadUrl :: L8.ByteString -> m UploadUrlResponse,
    parseUploadFile :: L8.ByteString -> m UploadFileResponse,
    parseUploadObject :: L8.ByteString -> m UploadObjectResponse,
    
    getLastSucUpdate :: m (Maybe UpdateId),
    putUpdate :: UpdateId -> m (),
    getRepliesNumber :: UserId -> m RepNum,
    setRepliesNumber :: UserId -> RepNum -> m (),
    getMode :: UserId -> m Text,
    setMode :: UserId -> Mode -> m (),

    sendNEchoMessage :: UserId -> Text -> Maybe [Attachment] ->
                        Maybe Geo -> RepNum -> m (),
    sendRepeatMessage :: UserId -> m (),
    sendHelpMessage :: UserId -> m (),
    updateAttachments :: Maybe [Attachment] -> m (Maybe [Attachment]),
    getUpdate :: Text -> Text -> Integer -> m B.ByteString,
    getServer :: m B.ByteString
}

{-- | run Vk bot --}
run :: (MonadThrow m, Monad m) => Handle m -> m ()
run handle = do
  let logH = hLogger handle
  Logger.logInfo logH "Bot api: vk"
  -- Connect to DB
  serverUp <- getServer handle
  serverParamsE <- EiT.runEitherT $ do
    params <- EiT.EitherT $ parsePollResponse handle serverUp
    let tsText = Server.text_ts $ PollResp.response params
    tsInt <- EiT.EitherT $ readEitherMa tsText
    return Server {
      Server.key = Server.text_key $ PollResp.response params,
      Server.server = Server.text_server $ PollResp.response params,
      Server.ts = tsInt
    }
  case serverParamsE of
    Left msg2 -> throwM $ E.ParseRequestError $ T.unpack msg2
    Right serverParams -> checkMode handle serverParams

checkMode :: Monad m => Handle m -> Server -> m ()
checkMode handle serverParams = do
  let logH = hLogger handle
      -- Extract server 
      serverLink = Server.server serverParams
      serverKey = Server.key serverParams
      tsCurrent = Server.ts serverParams
  -- Get last successfully processed update from DB
  processedUpdId <- getLastSucUpdate handle
  -- Get updates from bot
  let newUpdId = fmap (+1) processedUpdId
      tsDb = fromMaybe tsCurrent newUpdId
  Logger.logInfo logH ("Work with update: " <> convert tsDb)
  responseUp <- getUpdate handle serverLink serverKey tsDb
  updateData <- parseUpdateData handle responseUp
  -- Extract result (updates) from updatesData:
  --          if no new messages print Warning
  let update = updates updateData
  case update of
    [] -> do
      Logger.logWarning logH "Where are no more updates for now!"
      Logger.logInfo logH "Waiting updates!"
      checkMode handle serverParams {Server.ts = tsDb - 1}
    (x:_) -> do
      let updateType = Update.update_type x -- Extract u_type from Update
          message = Update.object x -- Extract Message from Update
          userId = Message.user_id message
      mode <- getMode handle userId
      let action | mode == Settings.reply && updateType == T.pack "message_new" = do
                    _ <- replyMode handle message
                    Logger.logDebug logH "Bot in reply mode."
                 | mode == Settings.answer && updateType == T.pack "message_new" = do
                    _ <- answerMode handle message
                    Logger.logDebug logH "Bot in answer mode."
                 | otherwise = Logger.logError logH "Unsupported type of update."
      action
      putUpdate handle tsDb
      checkMode handle serverParams {Server.ts = tsDb + 1}

replyMode :: Monad m => Handle m -> Message -> m Mode
replyMode handle message = do
  let logH = hLogger handle
      userId = Message.user_id message
      messageText = Message.body message
      messageAttachments = Message.attachments message
      messageGeo = Message.geo message
  attsNew <- updateAttachments handle messageAttachments
  Logger.logInfo logH $ "Checking message from user with id: " 
    <> convert userId
  let action | Settings.helpMessage == messageText = do 
                Logger.logInfo logH "User's /help message"
                sendHelpMessage handle userId
                return Settings.reply
             | Settings.repeatMessage == messageText = do
                sendRepeatMessage handle userId
                Logger.logInfo logH "User's /repeat message"
                setMode handle userId Settings.answer
                return Settings.answer
             | otherwise = do
                Logger.logInfo logH "It's text message from User."
                repNum <- getRepliesNumber handle userId
                sendNEchoMessage
                  handle userId messageText attsNew messageGeo repNum
                return Settings.reply
  action

answerMode :: Monad m => Handle m -> Message -> m (Maybe RepNum)
answerMode handle message = do
  let logH = hLogger handle
      userId = Message.user_id message
      messageText = T.unpack $ Message.body message
      -- Extract pollData_result (message) from updatesData:
      --     if no new messages print Warning
  setMode handle userId Settings.reply
  case (readMaybe messageText :: Maybe Integer) of
    Just repNum -> do
      Logger.logInfo logH "Info: Received user's answer"
      setRepliesNumber handle userId repNum
      return $ Just repNum
    Nothing -> do
      Logger.logError logH "Couldn't parse User's answer!"
      return Nothing
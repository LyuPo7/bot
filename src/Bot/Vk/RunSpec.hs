{-# LANGUAGE OverloadedStrings #-}

module Bot.Vk.RunSpec where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack, pack)

import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.DB.DBSpec as DB
import qualified Bot.Vk.Request.RequestsSpec as Req
import qualified Bot.Vk.Parser.ParserSpec as Parser
import Bot.Vk.Parser.Data

data Handle m = Handle {
    hLogger :: Logger.Handle m,
    cRun :: Settings.Config,
    hDb :: DB.Handle m,
    hReq :: Req.Handle m,
    hParser :: Parser.Handle m,
    
    parsePollResponse :: L8.ByteString -> m PollResponse,
    parseUpdateData :: L8.ByteString -> m UpdateData,
    parseUploadUrl :: L8.ByteString -> m UploadUrlResponse,
    parseUploadFile :: L8.ByteString -> m UploadFileResponse,
    parseUploadObject :: L8.ByteString -> m UploadObjectResponse,
    
    getLastSucUpdate :: m (Maybe UpdateID),
    putUpdate :: UpdateID -> m (),
    getRepliesNumber :: UserID -> m RepNum,
    setRepliesNumber :: UserID -> RepNum -> m (),
    getMode :: UserID -> m Text,
    setMode :: UserID -> Mode -> m (),

    sendNEchoMessage :: UserID -> Text -> Maybe [Attachment] -> Maybe Geo -> RepNum -> m (),
    sendRepeatMessage :: UserID -> m (),
    sendHelpMessage :: UserID -> m (),
    updateAttachments :: Maybe [Attachment] -> m (Maybe [Attachment]),
    getUpdate :: Text -> Text -> Integer -> m B.ByteString,
    getServer :: m B.ByteString
}

{-- | run Vk bot --}
run :: Monad m => Handle m -> m ()
run handle = do
  let logh = hLogger handle
  Logger.logInfo logh "Bot api: vk"
  -- Connect to DB
  serverUp <- getServer handle
  params <- parsePollResponse handle serverUp
  let serverParams = pollResponse_response params
  checkMode handle serverParams

checkMode :: Monad m => Handle m -> Server -> m ()
checkMode handle serverParams = do
  let logh = hLogger handle
      -- Extract server 
      server = server_server serverParams
      key = server_key serverParams
  -- let ts = server_ts serverParams
  -- Get last successfully processed update from DB
  processedUpdId <- getLastSucUpdate handle
  -- Get updates from bot
  let newUpdId = fmap (+1) processedUpdId
      tsDb = fromMaybe 0 newUpdId
  Logger.logInfo logh ("Work with update: " <> convert tsDb)
  responseUp <- getUpdate handle server key tsDb
  updateData <- parseUpdateData handle responseUp
  -- Extract result (updates) from updatesData: if no new messages print Warning
  let update = updates updateData
  case update of
    [] -> do
      Logger.logWarning logh "Where are no more updates for now!"
      Logger.logInfo logh "Waiting updates!"
      checkMode handle serverParams
    (x:_) -> do
      let updateType = update_type x -- Extract update_type from Update
          message = update_object x -- Extract Message from Update
          userId = message_userId message
      mode <- getMode handle userId
      let action | mode == Settings.reply && updateType == pack "message_new" = do
                    Logger.logDebug logh "Bot in reply mode."
                    replyMode handle message
                 | mode == Settings.answer && updateType == pack "message_new" = do
                    Logger.logDebug logh "Bot in answer mode."
                    answerMode handle message
                 | otherwise = Logger.logError logh "Unsuported type of update."
      action
      putUpdate handle tsDb
      checkMode handle serverParams {server_ts = tsDb + 1}

replyMode :: Monad m => Handle m -> Message -> m ()
replyMode handle message = do
  let logh = hLogger handle
      userId = message_userId message
      messageText = message_body message
      attachments = message_attachments message
      geo = message_geo message
  attachmentsNew <- updateAttachments handle attachments
  Logger.logInfo logh ("Checking message from user with id: " <> convert userId)
  let action | Settings.helpMessage == messageText = do 
                Logger.logInfo logh "User's /help message"
                sendHelpMessage handle userId
             | Settings.repeatMessage == messageText = do
                sendRepeatMessage handle userId
                Logger.logInfo logh "User's /repeat message"
                setMode handle userId Settings.answer
             | otherwise = do
                Logger.logInfo logh "It's text message from User."
                repNum <- getRepliesNumber handle userId
                sendNEchoMessage handle userId messageText attachmentsNew geo repNum
  action

answerMode :: Monad m => Handle m -> Message -> m ()
answerMode handle message = do
  let logh = hLogger handle
      userId = message_userId message
      messageText = unpack $ message_body message
      -- Extract pollData_result (message) from updatesData: if no new messages print Warning
      action | all isDigit messageText = do 
                Logger.logInfo logh "Info: Recieved user's answer"
                setRepliesNumber handle userId (read messageText :: Integer)
             | otherwise = Logger.logWarning logh "Wrong answer from user"
  action
  setMode handle userId Settings.reply

convert :: Show a => a -> Text
convert = pack . show
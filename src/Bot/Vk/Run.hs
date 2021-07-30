{-# LANGUAGE OverloadedStrings #-}

module Bot.Vk.Run where

import Database.HDBC (IConnection) 
import Data.Maybe (fromMaybe)
import Data.Text (unpack, pack)
import Data.Char (isDigit)

import qualified Bot.Logger as BL

import qualified Bot.Config ()
import qualified Bot.Logger ()
import Bot.DB (connect, getLastSucUpdate, getMode, getRepliesNumber, putUpdate, setMode, setRepliesNumber)
import Bot.Settings (helpMessage, repeatMessage, answer, reply)
import Bot.Vk.Parser.Parser (parsePollResponse, parseUpdateData)
import Bot.Vk.Request.Requests (getServer, getUpdate, sendNEchoMessage, sendRepeatMessage, sendHelpMessage, updateAttachments)
import Bot.Vk.Parser.Data (Server(..), Message(..), Update(..), UpdateData(..), PollResponse(..))

run :: BL.Handle -> IO ()
run logh = do
  BL.logInfo logh "Bot api: vk"
  -- Connect to DB
  dbh <- connect logh "src/Bot/files/test_vk.db"
  params <- parsePollResponse logh $ getServer logh
  let serverParams = pollResponse_response params
  checkMode dbh logh serverParams

checkMode :: IConnection conn => conn -> BL.Handle -> Server -> IO ()
checkMode dbh logh serverParams = do
  -- Extract server 
  let server = server_server serverParams
  let key = server_key serverParams
  --let ts = server_ts serverParams
  -- Get last successfully processed update from DB
  processedUpdId <- getLastSucUpdate logh dbh
  -- Get updates from bot
  let newUpdId = fmap (+1) processedUpdId
  let tsDb = fromMaybe 0 newUpdId
  BL.logInfo logh ("Work with update: " ++ show tsDb)
  updateData <- parseUpdateData logh (getUpdate logh server key tsDb)
  -- Extract result (updates) from updatesData: if no new messages print Warning
  let update = updates updateData
  case update of
    [] -> do
      BL.logWarning logh "Where are no more updates for now!"
      BL.logInfo logh "Waiting updates!"
      checkMode dbh logh serverParams
    (x:_) -> do
      let updateType = update_type x -- Extract update_type from Update
      let message = update_object x -- Extract Message from Update
      let userId = message_userId message
      mode <- getMode logh dbh userId
      let action | mode == reply && updateType == pack "message_new" = do
                    BL.logDebug logh "Bot in reply mode."
                    replyMode logh dbh message
                 | mode == answer && updateType == pack "message_new" = do
                    BL.logDebug logh "Bot in answer mode."
                    answerMode logh dbh message
                 | otherwise = BL.logError logh "Unsuported type of update."
      action
      putUpdate logh dbh tsDb
      checkMode dbh logh serverParams {server_ts = tsDb + 1}

replyMode :: IConnection conn => BL.Handle -> conn -> Message -> IO ()
replyMode logh dbh message = do
  let userId = message_userId message
  let messageText = message_body message
  let attachments = message_attachments message
  let geo = message_geo message
  attachmentsNew <- updateAttachments logh attachments
  BL.logInfo logh ("Checking message from user with id: " ++ show userId)
  let action | helpMessage == messageText = do 
                BL.logInfo logh "User's /help message"
                sendHelpMessage logh userId
             | repeatMessage == messageText = do
                sendRepeatMessage logh userId
                BL.logInfo logh "User's /repeat message"
                setMode logh dbh userId answer
             | otherwise = do
                BL.logInfo logh "It's text message from User."
                repNum <- getRepliesNumber logh dbh userId
                sendNEchoMessage logh userId messageText attachmentsNew geo repNum
  action

answerMode :: IConnection conn => BL.Handle -> conn -> Message -> IO ()
answerMode logh dbh message = do
  let userId = message_userId message
  let messageText = unpack $ message_body message
  -- Extract pollData_result (message) from updatesData: if no new messages print Warning
  let action | all isDigit messageText = do 
                BL.logInfo logh "Info: Recieved user's answer"
                setRepliesNumber logh dbh userId (read messageText :: Integer)
             | otherwise = BL.logWarning logh "Wrong answer from user"
  action
  setMode logh dbh userId reply
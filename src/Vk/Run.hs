{-# LANGUAGE OverloadedStrings #-}

module Vk.Run where

import Database.HDBC (IConnection) 
import Database.HDBC.Sqlite3 (Connection)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, unpack, pack)
import Control.Monad (when)
import Data.Char (isDigit)

import Vk.Types
import Settings 
import DB
import Vk.Requests
import Vk.Parser
import Logger
import LoggerIO

main :: IO ()
main = do
    -- handle log
  logh <- newHandle
  -- Connect to DB
  dbh <- connect logh "src/files/test_vk.db"
  params <- parsePollResponse logh $ getServer logh
  let serverParams = pollResponse_response params
  checkMode dbh logh serverParams

checkMode :: IConnection conn => conn -> Logger.Handle -> Server -> IO ()
checkMode dbh logh serverParams = do
  -- Extract server 
  let server = server_server serverParams
  let key = server_key serverParams
  let ts = server_ts serverParams
  -- Get last successfully processed update from DB
  processedUpdId <- getLastSucUpdate logh dbh
  -- Get updates from bot
  let newUpdId = fmap (+1) processedUpdId
  let tsDb = fromMaybe 0 newUpdId
  logInfo logh ("Work with update: " ++ show tsDb)
  updateData <- parseUpdateData logh (getUpdate logh server key tsDb)
  -- Extract result (updates) from updatesData: if no new messages print Warning
  let update = updates updateData
  case update of
    [] -> do
      logWarning logh "Where are no more updates for now!"
      logInfo logh "Waiting updates!"
      checkMode dbh logh serverParams
    (x:xs) -> do
      let updateEvent = update_eventId x -- Extract update_event from Update
      let updateType = update_type x -- Extract update_type from Update
      let message = update_object x -- Extract Message from Update
      let userId = message_userId message
      mode <- getMode logh dbh userId
      let action | mode == reply && updateType == pack "message_new" = do
                    logDebug logh "Bot in reply mode."
                    replyMode logh dbh message
                 | mode == answer && updateType == pack "message_new" = do
                    logDebug logh "Bot in answer mode."
                    answerMode logh dbh message
                 | otherwise = logError logh "Unsuported type of update."
      action
      putUpdate logh dbh tsDb
      checkMode dbh logh serverParams {server_ts = tsDb + 1}

replyMode :: IConnection conn => Logger.Handle -> conn -> Message -> IO ()
replyMode logh dbh message = do
  let userId = message_userId message
  let messageText = message_body message
  let attachments = message_attachments message
  let geo = message_geo message
  attachmentsNew <- updateAttachments logh attachments
  logInfo logh ("Checking message from user with id: " ++ show userId)
  let action | helpMessage == messageText = do 
                logInfo logh "User's /help message"
                sendHelpMessage logh userId
             | repeatMessage == messageText = do
                sendRepeatMessage logh userId
                logInfo logh "User's /repeat message"
                setMode logh dbh userId answer
             | otherwise = do
                logInfo logh "It's text message from User."
                repNum <- getRepliesNumber logh dbh userId
                sendNEchoMessage logh userId messageText attachmentsNew geo repNum
  action

answerMode :: IConnection conn => Logger.Handle -> conn -> Message -> IO ()
answerMode logh dbh message = do
  let userId = message_userId message
  let messageText = unpack $ message_body message
  -- Extract pollData_result (message) from updatesData: if no new messages print Warning
  let action | all isDigit messageText = do 
                logInfo logh "Info: Recieved user's answer"
                setRepliesNumber logh dbh userId (read messageText :: Integer)
             | otherwise = logWarning logh "Wrong answer from user"
  action
  setMode logh dbh userId reply
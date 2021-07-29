{-# LANGUAGE OverloadedStrings #-}

module Tele.Run where

import Control.Monad (when)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Database.HDBC (IConnection)
import Database.HDBC.Sqlite3 (Connection)

import Config
import DB
  ( connect
  , getLastSucUpdate
  , getMode
  , getRepliesNumber
  , putUpdate
  , setMode
  , setRepliesNumber
  )
import Logger
import LoggerIO
import Settings
  ( answer
  , config
  , helpMessage
  , repeatMessage
  , reply
  , startMessage
  )
import Tele.Parser (parseBotConfig, parsePollData, parseUpdateData)
import Tele.Requests
  ( getUpdate
  , sendNEchoMessage
  , sendQueryNumber
  , sendTextMessage
  )
import Tele.Types

main :: IO ()
main
  -- handle log
 = do
  logh <- newHandle
  -- Connect to DB
  dbh <- connect logh "src/files/test_tele.db"
  checkMode logh dbh

checkMode :: IConnection conn => Logger.Handle -> conn -> IO ()
checkMode logh dbh
  -- Get last successfully processed update from DB
 = do
  processedUpdId <- getLastSucUpdate logh dbh
  -- Get updates from bot
  let newUpdId = fmap (+ 1) processedUpdId
  updateData <- parseUpdateData logh (getUpdate logh newUpdId)
  -- Extract result (updates) from updatesData: if no new messages print Warning
  let update = result updateData
  case update of
    [] -> do
      logWarning logh "Where are no more updates for now!"
      logInfo logh "Waiting updates!"
      checkMode logh dbh
    [x] -> do
      let updateId = update_updateId x -- Extract update_id from Update
      let checkMessage = update_message x -- Extract message from Update
      logInfo logh $ "Checking update with id: " ++ show updateId
      case checkMessage of
        Nothing -> logWarning logh "Where are no new messages!"
        Just message
          -- check if in MessageEntities appears "bot_command"
         -> do
          let user = fromJust $ message_from message -- Extract user_id from Message
          let userId = user_id user
          mode <- getMode logh dbh userId
          let action
                | mode == reply = do
                  logDebug logh "Bot in reply mode."
                  replyMode logh dbh message
                | mode == answer = do
                  logDebug logh "Bot in answer mode."
                  answerMode logh dbh message
          action
          putUpdate logh dbh updateId
          checkMode logh dbh

replyMode :: IConnection conn => Logger.Handle -> conn -> Message -> IO ()
replyMode logh dbh message = do
  let messageType =
        filter ((== "bot_command") . messageent_type) <$>
        message_entities message
  let chatId = chat_id $ message_chat message -- Extract chat_id from Message
  let messageId = message_messageId message -- Extract message_id from Message
  let user = fromJust $ message_from message -- Extract user from Message
  let userId = user_id user -- Extract user_id from User
  let messageText = message_text message -- Extract text from Message
  case messageType of
    Nothing -> do
      repNum <- getRepliesNumber logh dbh userId
      sendNEchoMessage logh chatId messageId repNum
      logInfo logh "It's text message from User."
    Just z -> do
      let action
            | Just helpMessage == messageText = do
              logInfo logh "User's /help message"
              description <- fmap botDescription config
              sendTextMessage logh chatId description
            | Just repeatMessage == messageText = do
              quetion <- fmap botQuestion config
              sendQueryNumber logh chatId quetion
              logInfo logh "Info: User's /repeat message"
              setMode logh dbh userId answer
            | Just startMessage == messageText = do
              logInfo logh "User's /start message"
              sendTextMessage logh chatId "You are welcome!"
            | otherwise = do
              logError logh "Recieved unsupported bot-command!" --ignore it
      action

answerMode :: IConnection conn => Logger.Handle -> conn -> Message -> IO ()
answerMode logh dbh message = do
  let messageId = message_messageId message
  let user = fromJust $ message_from message -- Extract user_id from Message
  let userId = user_id user
  let messageText = unpack $ fromJust $ message_text message -- Extract text from Message
  when (all isDigit messageText) $ do
    let repNum = read messageText :: Integer
    logInfo logh $ "Info: Recieved user's answer: " ++ show messageId
    setRepliesNumber logh dbh userId repNum
    --logError logh "Couldn't parse User's answer!"
  setMode logh dbh userId reply

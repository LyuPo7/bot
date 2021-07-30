{-# LANGUAGE OverloadedStrings #-}

module Bot.Tele.Run where

import Control.Monad (when)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Database.HDBC (IConnection)

import qualified Bot.Config as Config
import qualified Bot.Logger as BL
import Bot.DB (connect, getLastSucUpdate, getMode, getRepliesNumber, putUpdate, setMode, setRepliesNumber)
import Bot.Settings (config, helpMessage, repeatMessage, startMessage, answer, reply)
import Bot.Tele.Parser.Parser (parseUpdateData)
import Bot.Tele.Request.Requests (getUpdate, sendNEchoMessage, sendQueryNumber, sendTextMessage)
import Bot.Tele.Parser.Data (Message(..), User(..), Chat(..), MessageEntity(..), Update(..), UpdateData(..))

{-- | run Tele bot --}
run :: BL.Handle -> IO ()
run logh = do
  BL.logInfo logh "Bot api: telegram"
  -- Connect to DB
  dbh <- connect logh "src/Bot/files/test_tele.db"
  -- Getting updates
  checkMode logh dbh

{-- | Check update mode --}
checkMode :: IConnection conn => BL.Handle -> conn -> IO ()
checkMode logh dbh = do
  -- Get last successfully processed update from DB
  processedUpdId <- getLastSucUpdate logh dbh
  let newUpdId = fmap (+ 1) processedUpdId -- set next update to check
  -- Get updates from api
  updateData <- parseUpdateData logh (getUpdate logh newUpdId)
  -- Extract result (updates) from updatesData: if no new messages print Warning
  let update = result updateData
  case update of
    [] -> do
      BL.logWarning logh "Where are no more updates for now!"
      BL.logInfo logh "Waiting updates!"
      checkMode logh dbh
    [x] -> do
      let updateId = update_updateId x -- Extract update_id from Update
      let checkMessage = update_message x -- Extract message from Update
      BL.logInfo logh $ "Checking update with id: " ++ show updateId
      case checkMessage of
        Nothing -> BL.logWarning logh "Where are no new messages!"
        Just message -> do
          -- check if in MessageEntities appears "bot_command"
          let user = fromJust $ message_from message -- Extract User from Message
          let userId = user_id user -- Extract user_id from User
          mode <- getMode logh dbh userId -- Get current work mode for this User
          let action -- realise depending on mode
                | mode == reply = do
                  BL.logDebug logh "Bot in reply mode."
                  replyMode logh dbh message
                | mode == answer = do
                  BL.logDebug logh "Bot in answer mode."
                  answerMode logh dbh message
                | otherwise = BL.logError logh "Unknown Bot mode."
          action
          putUpdate logh dbh updateId -- put update_id in db
          checkMode logh dbh
    _ -> BL.logError logh "Expected one update, but have some."

{-- | Reply mode --}
replyMode :: IConnection conn => BL.Handle -> conn -> Message -> IO ()
replyMode logh dbh message = do
  let messageType = filter ((== "bot_command") . messageent_type) <$> message_entities message
  let chatId = chat_id $ message_chat message -- Extract chat_id from Message
  let messageId = message_messageId message -- Extract message_id from Message
  let user = fromJust $ message_from message -- Extract User from Message
  let userId = user_id user -- Extract user_id from User
  let messageText = message_text message -- Extract text from Message
  case messageType of
    Nothing -> do
      repNum <- getRepliesNumber logh dbh userId -- Get current replyNum mode for this User
      sendNEchoMessage logh chatId messageId repNum
      BL.logInfo logh "It's ordinary message from User."
    Just _ -> do
      let action
            | Just helpMessage == messageText = do
              BL.logInfo logh "User's /help message"
              description <- fmap Config.botDescription config -- Get bot description from config
              sendTextMessage logh chatId description
            | Just repeatMessage == messageText = do
              quetion <- fmap Config.botQuestion config -- Get bot quetion from config
              _ <- sendQueryNumber logh chatId quetion
              BL.logInfo logh "Info: User's /repeat message"
              setMode logh dbh userId answer
            | Just startMessage == messageText = do
              BL.logInfo logh "User's /start message"
              sendTextMessage logh chatId "You are welcome!" -- Welcome message for User
            | otherwise = do
              BL.logError logh "Recieved unsupported bot-command!" -- Ignore it
      action

{-- | Waiting answer mode --}
answerMode :: IConnection conn => BL.Handle -> conn -> Message -> IO ()
answerMode logh dbh message = do
  let messageId = message_messageId message
  let user = fromJust $ message_from message -- Extract User from Message
  let userId = user_id user -- Extract user_id from User
  let messageText = unpack $ fromJust $ message_text message -- Extract text from Message
  when (all isDigit messageText) $ do
    let repNum = read messageText :: Integer -- Trying parse answer
    BL.logInfo logh $ "Info: Recieved user's answer: " ++ show messageId
    setRepliesNumber logh dbh userId repNum -- set new replyNum to db
    --logError logh "Couldn't parse User's answer!"
  setMode logh dbh userId reply

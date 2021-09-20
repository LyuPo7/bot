{-# LANGUAGE OverloadedStrings #-}

module Bot.Tele.RunSpec where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.DB.DBSpec as DBSpec
import qualified Bot.Tele.Request.RequestsSpec as ReqSpec
import qualified Bot.Tele.Parser.ParserSpec as ParserSpec
import Bot.Tele.Parser.Data
import Bot.Util (convert)

data Handle m = Handle {
    hLogger :: Logger.Handle m,
    cRun :: Settings.Config,
    hDb :: DBSpec.Handle m,
    hReq :: ReqSpec.Handle m,
    hParser :: ParserSpec.Handle m,
    
    parseUpdateData :: L8.ByteString -> m UpdateData,
    
    getLastSucUpdate :: m (Maybe UpdateID),
    putUpdate :: UpdateID -> m (),
    getRepliesNumber :: ChatID -> m RepNum,
    setRepliesNumber :: ChatID -> RepNum -> m (),
    getMode :: ChatID -> m Text,
    setMode :: ChatID -> Mode -> m (),

    getUpdate :: Maybe UpdateID -> m L8.ByteString,
    sendTextMessage :: ChatID -> Text -> m (),
    sendEchoMessage :: ChatID -> MessageID -> m (),
    sendNEchoMessage :: ChatID -> MessageID -> RepNum -> m (),
    sendQueryNumber :: ChatID -> Text -> m L8.ByteString,
    setCommands :: m ()
}

{-- | run Tele bot --}
run :: Monad m => Handle m -> m ()
run handle = do
  let logh = hLogger handle
  Logger.logInfo logh "Bot api: telegram"
  -- Set ommands for bot
  setCommands handle
  -- Getting updates
  _ <- checkMode handle
  return ()

{-- | Check update mode --}
checkMode :: Monad m => Handle m -> m (Maybe UpdateID)
checkMode handle = do
  let logh = hLogger handle
  -- Get last successfully processed update from DB
  processedUpdId <- getLastSucUpdate handle
  let newUpdId = fmap (+ 1) processedUpdId -- set next update to check
  -- Get updates from api
  responseUp <- getUpdate handle newUpdId
  updateData <- parseUpdateData handle responseUp
  -- Extract result (updates) from updatesData: if no new messages print Warning
  let update = result updateData
  case update of
    [] -> do
      Logger.logWarning logh "Where are no more updates for now!"
      Logger.logInfo logh "Waiting updates!"
      _ <- checkMode handle
      return Nothing
    [x] -> do
      let updateId = update_updateId x -- Extract update_id from Update
          checkMessage = update_message x -- Extract message from Update
      Logger.logInfo logh $ "Checking update with id: " <> convert updateId
      case checkMessage of
        Nothing -> do 
          Logger.logWarning logh "Where are no new messages!"
          return $ Just updateId
        Just message -> do
          -- check if in MessageEntities appears "bot_command"
          let chatId = chat_id $ message_chat message -- Extract chat_id from Message
          mode <- getMode handle chatId -- Get current work mode for this User
          let action -- realise depending on mode
                | mode == Settings.reply = do
                  _ <- replyMode handle message
                  Logger.logDebug logh "Bot in reply mode."
                | mode == Settings.answer = do
                  _ <- answerMode handle message
                  Logger.logDebug logh "Bot in answer mode."
                | otherwise = Logger.logError logh "Unknown Bot mode."
          action
          putUpdate handle updateId -- put update_id in db
          _ <- checkMode handle
          return $ Just updateId
    _ -> do
      Logger.logError logh "Expected one update, but have many."
      _ <- checkMode handle
      return Nothing

{-- | Reply mode --}
replyMode :: Monad m => Handle m -> Message -> m Mode
replyMode handle message = do
  let logh = hLogger handle
      config = cRun handle
      messageType = filter ((== "bot_command") . messageent_type) <$> message_entities message
      chatId = chat_id $ message_chat message -- Extract chat_id from Message
      messageId = message_messageId message -- Extract message_id from Message
      messageText = message_text message -- Extract text from Message
  case messageType of
    Nothing -> do
      repNum <- getRepliesNumber handle chatId -- Get current replyNum mode for this Chat
      sendNEchoMessage handle chatId messageId repNum
      Logger.logInfo logh "It's ordinary message from User."
      return Settings.reply
    Just _ -> do
      let action
            | Just Settings.helpMessage == messageText = do
              Logger.logInfo logh "User's /help message"
              let description = Settings.botDescription config -- Get bot description from config
              sendTextMessage handle chatId description
              return Settings.reply
            | Just Settings.repeatMessage == messageText = do
              let quetion = Settings.botQuestion config -- Get bot quetion from config
              _ <- sendQueryNumber handle chatId quetion
              Logger.logInfo logh "Info: User's /repeat message"
              setMode handle chatId Settings.answer
              return Settings.answer
            | Just Settings.startMessage == messageText = do
              Logger.logInfo logh "User's /start message"
              sendTextMessage handle chatId "You are welcome!" -- Welcome message for User
              return Settings.reply
            | otherwise = do
              Logger.logError logh "Recieved unsupported bot-command!" -- Ignore it
              return Settings.reply
      action

{-- | Waiting answer mode --}
answerMode :: Monad m => Handle m -> Message -> m (Maybe RepNum)
answerMode handle message = do
  let logh = hLogger handle
      messageId = message_messageId message
      chatId = chat_id $ message_chat message -- Extract chat_id from Message
      messageText = T.unpack $ fromMaybe "" $ message_text message -- Extract text from Message
  setMode handle chatId Settings.reply
  case (readMaybe messageText :: Maybe Integer) of -- Trying parse answer
    Just repNum -> do
      Logger.logInfo logh $ "Info: Recieved user's answer: " <> convert messageId
      setRepliesNumber handle chatId repNum -- set new replyNum to db
      return $ Just repNum
    Nothing -> do
      Logger.logError logh "Couldn't parse User's answer!"
      return Nothing
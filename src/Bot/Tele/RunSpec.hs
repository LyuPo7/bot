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
import qualified Bot.Tele.Parser.Objects.Chat as Chat
import qualified Bot.Tele.Parser.Objects.Update as Update
import Bot.Tele.Parser.Objects.UpdateData (UpdateData(..))
import Bot.Tele.Parser.Objects.Update (Update(..))
import Bot.Tele.Parser.Objects.Message (Message(..))
import Bot.Tele.Parser.Objects.MessageEntity (MessageEntity(..))
import Bot.Tele.Parser.Objects.Synonyms (RepNum, UpdateId, Mode, ChatId, MessageId)
import Bot.Util (convert)

data Handle m = Handle {
  hLogger :: Logger.Handle m,
  cRun :: Settings.Config,
  hDb :: DBSpec.Handle m,
  hReq :: ReqSpec.Handle m,
  hParser :: ParserSpec.Handle m,
  
  parseUpdateData :: L8.ByteString -> m UpdateData,
  
  getLastSucUpdate :: m (Maybe UpdateId),
  putUpdate :: UpdateId -> m (),
  getRepliesNumber :: ChatId -> m RepNum,
  setRepliesNumber :: ChatId -> RepNum -> m (),
  getMode :: ChatId -> m Text,
  setMode :: ChatId -> Mode -> m (),

  getUpdate :: Maybe UpdateId -> m L8.ByteString,
  sendTextMessage :: ChatId -> Text -> m (),
  sendEchoMessage :: ChatId -> MessageId -> m (),
  sendNEchoMessage :: ChatId -> MessageId -> RepNum -> m (),
  sendQueryNumber :: ChatId -> Text -> m L8.ByteString,
  setCommands :: m ()
}

{-- | run Tele bot --}
run :: Monad m => Handle m -> m ()
run handle = do
  let logH = hLogger handle
  Logger.logInfo logH "Bot api: telegram"
  -- Set commands for bot
  setCommands handle
  -- Getting updates
  _ <- checkMode handle
  return ()

{-- | Check update mode --}
checkMode :: Monad m => Handle m -> m (Maybe UpdateId)
checkMode handle = do
  let logH = hLogger handle
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
      Logger.logWarning logH "Where are no more updates for now!"
      Logger.logInfo logH "Waiting updates!"
      _ <- checkMode handle
      return Nothing
    [x] -> do
      let updateId = Update.id x -- Extract id from Update
          checkMessage = message x -- Extract message from Update
      Logger.logInfo logH $ "Checking update with id: " <> convert updateId
      case checkMessage of
        Nothing -> do 
          Logger.logWarning logH "Where are no new messages!"
          return $ Just updateId
        Just userMessage -> do
          -- check if in MessageEntities appears "bot_command"
          let chatId = Chat.id $ chat userMessage -- Extract chat_id
          mode <- getMode handle chatId -- Get current work mode User
          let action -- realise depending on mode
                | mode == Settings.reply = do
                  _ <- replyMode handle userMessage
                  Logger.logDebug logH "Bot in reply mode."
                | mode == Settings.answer = do
                  _ <- answerMode handle userMessage
                  Logger.logDebug logH "Bot in answer mode."
                | otherwise = Logger.logError logH "Unknown Bot mode."
          action
          putUpdate handle updateId -- put id in db
          _ <- checkMode handle
          return $ Just updateId
    _ -> do
      Logger.logError logH "Expected one update, but have many."
      _ <- checkMode handle
      return Nothing

{-- | Reply mode --}
replyMode :: Monad m => Handle m -> Message -> m Mode
replyMode handle userMessage = do
  let logH = hLogger handle
      config = cRun handle
      messageType = filter 
        ((== "bot_command") . entity_type) 
        <$> entities userMessage
      chatId = Chat.id $ chat userMessage -- Extract chat_id from Message
      messageId = message_id userMessage -- Extract id from Message
      messageText = text userMessage -- Extract text from Message
  case messageType of
    Nothing -> do
      repNum <- getRepliesNumber handle chatId -- Get current replyNum mode Chat
      sendNEchoMessage handle chatId messageId repNum
      Logger.logInfo logH "It's ordinary message from User."
      return Settings.reply
    Just _ -> do
      let action
            | Just Settings.helpMessage == messageText = do
              Logger.logInfo logH "User's /help message"
              let description = Settings.botDescription config
              sendTextMessage handle chatId description
              return Settings.reply
            | Just Settings.repeatMessage == messageText = do
              let question = Settings.botQuestion config
              _ <- sendQueryNumber handle chatId question
              Logger.logInfo logH "Info: User's /repeat message"
              setMode handle chatId Settings.answer
              return Settings.answer
            | Just Settings.startMessage == messageText = do
              Logger.logInfo logH "User's /start message"
              sendTextMessage handle chatId "You are welcome!"
              return Settings.reply
            | otherwise = do
              -- Ignore it
              Logger.logError logH "Received unsupported bot-command!"
              return Settings.reply
      action

{-- | Waiting answer mode --}
answerMode :: Monad m => Handle m -> Message -> m (Maybe RepNum)
answerMode handle userMessage = do
  let logH = hLogger handle
      messageId = message_id userMessage
      -- Extract chat_id from Message
      chatId = Chat.id $ chat userMessage
      -- Extract text from Message
      messageText = T.unpack $ fromMaybe "" $ text userMessage 
  setMode handle chatId Settings.reply
  case (readMaybe messageText :: Maybe Integer) of -- Trying parse answer
    Just repNum -> do
      Logger.logInfo logH $ "Info: Received user's answer: "
        <> convert messageId
      setRepliesNumber handle chatId repNum -- set new replyNum to db
      return $ Just repNum
    Nothing -> do
      Logger.logError logH "Couldn't parse User's answer!"
      return Nothing
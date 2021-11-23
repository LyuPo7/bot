module Bot.Mode.Mode where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad.Catch (MonadThrow)

import qualified Bot.Logger.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.DB.DBQ as BotDBQ
import qualified Bot.Request.Request as BotReq
import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Update as BotUpdate
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Objects.MessageType as BotMessageType
import qualified Bot.Objects.Mode as BotMode
import qualified Bot.Util as BotUtil

data Handle m = Handle {
  hLogger :: Logger.Handle m,
  cRun :: Settings.Config,
  hDb :: BotDBQ.Handle m,
  hReq :: BotReq.Handle m,
  hParser :: BotParser.Handle m,

  setupBot :: m (),
  getFirstUpdate :: m BotUpdate.Update,
  getLastUpdate :: BotUpdate.Update -> m (Maybe (BotSynonyms.UpdateId, BotMessage.Message)),
  getNextUpdate :: BotUpdate.Update -> m BotUpdate.Update,
  getPrevUpdate :: BotUpdate.Update -> m BotUpdate.Update,
  getChatId :: BotMessage.Message -> m BotSynonyms.ChatId,
  getMessageText :: BotMessage.Message -> m (Maybe Text),
  getMessageType :: BotMessage.Message -> m BotMessageType.MessageType
}

startMode :: (MonadThrow m, Monad m) => Handle m -> m ()
startMode handle = do
  let logH = hLogger handle
      config = cRun handle
      apiName = Settings.botApi config
  Logger.logInfo logH $ "Bot api: "
    <> BotUtil.convertValue apiName
  setupBot handle
  firstUpdate <- getFirstUpdate handle
  _ <- checkMode handle firstUpdate
  return ()

checkMode :: (MonadThrow m, Monad m) => Handle m -> BotUpdate.Update -> m (Maybe BotSynonyms.UpdateId)
checkMode handle prevUpdate = do
  let logH = hLogger handle
      dbH = hDb handle
  nextUpdate <- getLastUpdate handle prevUpdate
  case nextUpdate of
    Nothing -> do
      Logger.logWarning logH "Where are no more updates for now!"
      Logger.logInfo logH "Waiting updates!"
      newUpdate <- getPrevUpdate handle prevUpdate
      _ <- checkMode handle newUpdate
      return Nothing
    Just (updateId, userMessage) -> do
      Logger.logInfo logH $ "Checking update with id: "
        <> BotUtil.convertValue updateId
      chatId <- getChatId handle userMessage
      mode <- BotDBQ.getMode dbH chatId
      case mode of
        BotMode.ReplyMode -> do
          _ <- replyMode handle userMessage
          Logger.logDebug logH "Bot in reply mode."
        BotMode.AnswerMode -> do
          _ <- answerMode handle userMessage
          Logger.logDebug logH "Bot in answer mode."
        BotMode.UnknownMode -> do
          Logger.logError logH $ "Unknown Bot mode: "
            <> BotUtil.convertValue mode
      BotDBQ.putUpdate dbH updateId
      newUpdate <- getNextUpdate handle prevUpdate
      _ <- checkMode handle newUpdate
      return $ Just updateId

replyMode :: (MonadThrow m, Monad m) => Handle m -> BotMessage.Message -> m BotMode.Mode
replyMode handle userMessage = do
  let logH = hLogger handle
      dbH = hDb handle
      reqH = hReq handle
  chatId <- getChatId handle userMessage
  messageType <- getMessageType handle userMessage
  case messageType of
    BotMessageType.TextMessage _ -> do
      repNum <- BotDBQ.getRepliesNumber dbH chatId
      BotReq.sendNEchoMessage reqH userMessage repNum
      Logger.logInfo logH "It's ordinary message from User."
      return BotMode.ReplyMode
    BotMessageType.HelpMessage -> do
      Logger.logInfo logH "User's /help message"
      _ <- BotReq.sendHelpMessage reqH userMessage
      return BotMode.ReplyMode
    BotMessageType.RepeatMessage -> do
      _ <- BotReq.sendKeyboard reqH userMessage
      Logger.logInfo logH "Info: User's /repeat message"
      BotDBQ.setMode dbH chatId BotMode.AnswerMode
      return BotMode.AnswerMode
    BotMessageType.StartMessage -> do
      Logger.logInfo logH "User's /start message"
      BotReq.sendStartMessage reqH userMessage
      return BotMode.ReplyMode
    BotMessageType.UnsupportedMessage -> do
      Logger.logError logH "Received unsupported bot-command!"
      return BotMode.ReplyMode

answerMode :: Monad m => Handle m -> BotMessage.Message ->
              m (Maybe BotSynonyms.RepNum)
answerMode handle userMessage = do
  let logH = hLogger handle
      dbH = hDb handle
  chatId <- getChatId handle userMessage
  messageTextM <- getMessageText handle userMessage
  let messageText = T.unpack $ fromMaybe "" messageTextM
  BotDBQ.setMode dbH chatId BotMode.ReplyMode
  case (readMaybe messageText :: Maybe BotSynonyms.RepNum) of
    Just repNum -> do
      Logger.logInfo logH $ "Info: Received user's answer in chat with id: "
        <> BotUtil.convertValue chatId
      BotDBQ.setRepliesNumber dbH chatId repNum
      return $ Just repNum
    Nothing -> do
      Logger.logError logH "Couldn't parse User's answer!"
      return Nothing
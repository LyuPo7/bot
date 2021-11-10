module Bot.Tele.Request.RequestsSpec where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)

import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Tele.Parser.ParserSpec as ParserSpec
import qualified Bot.Tele.Request.Objects.TeleRequest as TeleReq
import qualified Bot.Tele.Request.Objects.RequestOptions as ReqOptions
import qualified Bot.Tele.Request.Objects.InlineKeyboardButton as Button
import qualified Bot.Tele.Request.Objects.InlineKeyboardMarkup as Keyboard
import Bot.Tele.Request.Objects.TeleRequest (TeleRequest)
import Bot.Tele.Request.Objects.RequestOptions (RequestOptions)
import Bot.Tele.Parser.Objects.Synonyms (ChatId, RepNum, MessageId, UpdateId)
import Bot.Util (convert)

data Handle m = Handle {
  hLogger :: Logger.Handle m,
  configReq :: Settings.Config,
  hParser :: ParserSpec.Handle m,

  makeRequest :: TeleRequest -> RequestOptions -> m B.ByteString
}

{-- | Get update request --}
getUpdate :: Monad m => Handle m -> Maybe UpdateId -> m L8.ByteString
getUpdate handle updateId  = do
  -- Get JSON data
  let updateOptions = ReqOptions.createGetUpdates updateId
  makeRequest handle TeleReq.getUpdates updateOptions

{-- | Send text Message request --}
sendTextMessage :: Monad m => Handle m -> ChatId -> Text -> m ()
sendTextMessage handle chatId text = do
  let logH = hLogger handle
      message = ReqOptions.createSendMessage chatId text
  _ <- makeRequest handle TeleReq.sendMessage message
  Logger.logInfo logH $ "Message with text: " <> 
      text <> " was sended to chat with id: " <> 
      convert chatId

{-- | Copy Message request --}
sendEchoMessage :: Monad m => Handle m -> ChatId -> MessageId -> m ()
sendEchoMessage handle chatId messageId = do
  let logH = hLogger handle
      message = ReqOptions.createCopyMessage chatId messageId
  _ <- makeRequest handle TeleReq.copyMessage message
  Logger.logInfo logH $ "Echo-Message with id: " <>
      convert messageId <>
      " was forwarded to chat with id: " <> convert chatId

{-- | Copy(n times) Message request --}
sendNEchoMessage :: Monad m => Handle m -> ChatId -> MessageId -> RepNum -> m ()
sendNEchoMessage handle _ _ 0 = do
  let logH = hLogger handle
  Logger.logInfo logH "Echo-Messages were sended."
sendNEchoMessage handle chatId messageId n = do
  sendEchoMessage handle chatId messageId
  sendNEchoMessage handle chatId messageId (n - 1)

{-- | Send Message with InlineKeyboard --}
sendQueryNumber :: Monad m => Handle m -> ChatId -> Text -> m L8.ByteString
sendQueryNumber handle chatId question = do
  let logH = hLogger handle
      b1 = Button.createButton "1" "Pressed 1"
      b2 = Button.createButton "2" "Pressed 2"
      b3 = Button.createButton "3" "Pressed 3"
      b4 = Button.createButton "4" "Pressed 4"
      b5 = Button.createButton "5" "Pressed 5"
      markupIn = Keyboard.createKeyboard [[b1, b2, b3, b4, b5]]
      query = ReqOptions.createQueryMessage chatId question markupIn
  Logger.logInfo logH $ "Question was sended to chat with id: "
    <> convert chatId
  makeRequest handle TeleReq.sendMessage query

{-- | St Bot Commands "/help", "/repeat--}
setCommands :: Monad m => Handle m -> m ()
setCommands handle = do
  let logH = hLogger handle
      commands = ReqOptions.createBotCommands
  _ <- makeRequest handle TeleReq.setBotCommands commands
  Logger.logInfo logH "Bot commands: '/help', '/repeat' were created."
module Bot.Tele.Request.RequestsSpec where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)

import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Tele.Parser.ParserSpec as ParserSpec
import qualified Bot.Tele.Request.Data as RD
import Bot.Tele.Request.Data (TeleRequest, RequestOptions)
import Bot.Tele.Parser.Data (ChatID, RepNum, MessageID, UpdateID)
import Bot.Util (convert)

data Handle m = Handle {
  hLogger :: Logger.Handle m,
  configReq :: Settings.Config,
  hParser :: ParserSpec.Handle m,

  makeRequest :: TeleRequest -> RequestOptions -> m B.ByteString
}

{-- | Get update request --}
getUpdate :: Monad m => Handle m -> Maybe UpdateID -> m L8.ByteString
getUpdate handle updateId  = do
  -- Get JSON data
  let updateOptions = RD.createGetUpdates updateId
  makeRequest handle RD.getUpdates updateOptions

{-- | Send text Message request --}
sendTextMessage :: Monad m => Handle m -> ChatID -> Text -> m ()
sendTextMessage handle chatId text = do
  let logH = hLogger handle
      message = RD.createSendMessage chatId text
  _ <- makeRequest handle RD.sendMessage message
  Logger.logInfo logH $ "Message with text: " <> 
      text <> " was sended to chat with id: " <> 
      convert chatId

{-- | Copy Message request --}
sendEchoMessage :: Monad m => Handle m -> ChatID -> MessageID -> m ()
sendEchoMessage handle chatId messageId = do
  let logH = hLogger handle
      message = RD.createCopyMessage chatId messageId
  _ <- makeRequest handle RD.copyMessage message
  Logger.logInfo logH $ "Echo-Message with id: " <>
      convert messageId <>
      " was forwarded to chat with id: " <> convert chatId

{-- | Copy(n times) Message request --}
sendNEchoMessage :: Monad m => Handle m -> ChatID -> MessageID -> RepNum -> m ()
sendNEchoMessage handle _ _ 0 = do
  let logH = hLogger handle
  Logger.logInfo logH "Echo-Messages were sended."
sendNEchoMessage handle chatId messageId n = do
  sendEchoMessage handle chatId messageId
  sendNEchoMessage handle chatId messageId (n - 1)

{-- | Send Message with InlineKeyboard --}
sendQueryNumber :: Monad m => Handle m -> ChatID -> Text -> m L8.ByteString
sendQueryNumber handle chatId question = do
  let logH = hLogger handle
      b1 = RD.createButton "1" "Pressed 1"
      b2 = RD.createButton "2" "Pressed 2"
      b3 = RD.createButton "3" "Pressed 3"
      b4 = RD.createButton "4" "Pressed 4"
      b5 = RD.createButton "5" "Pressed 5"
      markupIn = RD.createKeyboard [[b1, b2, b3, b4, b5]]
      query = RD.createQueryMessage chatId question markupIn
  Logger.logInfo logH $ "Question was sended to chat with id: "
    <> convert chatId
  makeRequest handle RD.sendMessage query

{-- | St Bot Commands "/help", "/repeat--}
setCommands :: Monad m => Handle m -> m ()
setCommands handle = do
  let logH = hLogger handle
      commands = RD.createBotCommands
  _ <- makeRequest handle RD.setBotCommands commands
  Logger.logInfo logH "Bot commands: '/help', '/repeat' were created."
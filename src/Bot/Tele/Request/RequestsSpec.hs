{-# LANGUAGE OverloadedStrings #-}

module Bot.Tele.Request.RequestsSpec where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)

import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Tele.Parser.ParserSpec as ParserSpec
import Bot.Tele.Request.Data
import Bot.Tele.Parser.Data
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
  let updateOptions = createGetUpdates updateId
  makeRequest handle getUpdates updateOptions

{-- | Send text Message request --}
sendTextMessage :: Monad m => Handle m -> ChatID -> Text -> m ()
sendTextMessage handle chatId text = do
  let logh = hLogger handle
      message = createSendMessage chatId text
  _ <- makeRequest handle sendMessage message
  Logger.logInfo logh $ "Message with text: " <> 
      text <> " was sended to chat with id: " <> 
      convert chatId

{-- | Copy Message request --}
sendEchoMessage :: Monad m => Handle m -> ChatID -> MessageID -> m ()
sendEchoMessage handle chatId messageId = do
  let logh = hLogger handle
      message = createCopyMessage chatId messageId
  _ <- makeRequest handle copyMessage message
  Logger.logInfo logh $ "Echo-Message with id: " <>
      convert messageId <>
      " was forwarded to chat with id: " <> convert chatId

{-- | Copy(n times) Message request --}
sendNEchoMessage :: Monad m => Handle m -> ChatID -> MessageID -> RepNum -> m ()
sendNEchoMessage handle _ _ 0 = do
  let logh = hLogger handle
  Logger.logInfo logh "Echo-Messages were sended."
sendNEchoMessage handle chatId messageId n = do
  sendEchoMessage handle chatId messageId
  sendNEchoMessage handle chatId messageId (n - 1)

{-- | Send Message with InlineKeyboard --}
sendQueryNumber :: Monad m => Handle m -> ChatID -> Text -> m L8.ByteString
sendQueryNumber handle chatId question = do
  let logh = hLogger handle
      b1 = createButton "1" "Pressed 1"
      b2 = createButton "2" "Pressed 2"
      b3 = createButton "3" "Pressed 3"
      b4 = createButton "4" "Pressed 4"
      b5 = createButton "5" "Pressed 5"
      markupIn = createKeyboard [[b1, b2, b3, b4, b5]]
      query = createQueryMessage chatId question markupIn
  Logger.logInfo logh $ "Question was sended to chat with id: " <> convert chatId
  makeRequest handle sendMessage query

{-- | St Bot Commands "/help", "/repeat--}
setCommands :: Monad m => Handle m -> m ()
setCommands handle = do
  let logh = hLogger handle
      commands = createBotCommands
  _ <- makeRequest handle setBotCommands commands
  Logger.logInfo logh "Bot commands: '/help', '/repeat' were created."
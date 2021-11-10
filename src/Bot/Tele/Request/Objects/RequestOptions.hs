{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Tele.Request.Objects.RequestOptions where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..))

import qualified Bot.Settings as Settings
import Bot.Tele.Parser.Objects.Synonyms (ChatId, MessageId)
import Bot.Tele.Request.Objects.InlineKeyboardMarkup (InlineKeyboardMarkup(..))
import Bot.Tele.Request.Objects.BotCommandScope (BotCommandScope(..), defaultBotCommandScope)
import Bot.Tele.Request.Objects.BotCommand (BotCommand(..), repBotCommand, helpBotCommand)

data RequestOptions = 
  GetUpdates {
    offset :: Maybe Integer,
    limit :: Maybe Integer,
    timeout :: Maybe Integer,
    allowed_updates :: Maybe [Text]
  } | 
  SendMessage {
    chat_id :: ChatId,
    text :: Text,
    disable_notification :: Maybe Bool,
    reply_to_message_id :: Maybe MessageId
  } |
  QueryMessage {
    chat_id :: ChatId,
    text :: Text,
    disable_notification :: Maybe Bool,
    reply_to_message_id :: Maybe MessageId,
    reply_markup :: Maybe InlineKeyboardMarkup
  } |
  CopyMessage {
    chat_id :: ChatId,
    from_chat_id :: ChatId,
    message_id :: MessageId
  } |
  GetBotCommands {
    scope :: Maybe BotCommandScope,
    language_code :: Maybe Text -- A two-letter ISO 639-1 language code.
  } |
  SetBotCommands {
    commands :: [BotCommand],
    scope :: Maybe BotCommandScope,
    language_code :: Maybe Text -- A two-letter ISO 639-1 language code.
  } deriving (Show, Generic, ToJSON)

-- | Default methods for Requests
createGetUpdates :: Maybe Integer -> RequestOptions
createGetUpdates updateId = GetUpdates {
  offset = updateId,
  limit = Nothing,
  timeout = Just Settings.timeout,
  allowed_updates = Just ["message"]
}

createSendMessage :: Integer -> Text -> RequestOptions
createSendMessage chatId messageText = SendMessage {
  chat_id = chatId,
  text = messageText,
  disable_notification = Nothing,
  reply_to_message_id = Nothing
}

createCopyMessage :: Integer -> Integer -> RequestOptions
createCopyMessage chatId messageId = CopyMessage { 
  chat_id = chatId,
  from_chat_id = chatId,
  message_id = messageId
}

createQueryMessage :: Integer -> Text -> InlineKeyboardMarkup -> RequestOptions
createQueryMessage chatId question markupIn = QueryMessage {
  chat_id = chatId,
  text = question,
  disable_notification = Nothing,
  reply_to_message_id = Nothing,
  reply_markup = Just markupIn
}

createBotCommands :: RequestOptions
createBotCommands = SetBotCommands {
  commands = [helpBotCommand, repBotCommand],
  scope = Just defaultBotCommandScope,
  language_code = Just ""
}
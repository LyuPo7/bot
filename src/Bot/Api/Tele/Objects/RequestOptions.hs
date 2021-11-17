{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Tele.Objects.RequestOptions where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON(..))

import qualified Bot.Settings as Settings
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Api.Tele.Objects.Keyboard as TeleKeyboard
import qualified Bot.Api.Tele.Objects.CommandScope as TeleCommandScope
import qualified Bot.Api.Tele.Objects.Command as TeleCommand

data RequestOptions = 
  GetUpdates {
    offset :: Maybe BotSynonyms.Offset,
    limit :: Maybe BotSynonyms.RecordLimit,
    timeout :: Maybe BotSynonyms.Timeout,
    allowed_updates :: Maybe [Text]
  } | 
  SendMessage {
    chat_id :: BotSynonyms.ChatId,
    text :: Text,
    disable_notification :: Maybe Bool,
    reply_to_message_id :: Maybe BotSynonyms.MessageId
  } |
  QueryMessage {
    chat_id :: BotSynonyms.ChatId,
    text :: Text,
    disable_notification :: Maybe Bool,
    reply_to_message_id :: Maybe BotSynonyms.MessageId,
    reply_markup :: Maybe TeleKeyboard.Keyboard
  } |
  CopyMessage {
    chat_id :: BotSynonyms.ChatId,
    from_chat_id :: BotSynonyms.ChatId,
    message_id :: BotSynonyms.MessageId
  } |
  GetCommands {
    scope :: Maybe TeleCommandScope.CommandScope,
    language_code :: Maybe BotSynonyms.Language -- A two-letter ISO 639-1 language code.
  } |
  SetCommands {
    commands :: [TeleCommand.Command],
    scope :: Maybe TeleCommandScope.CommandScope,
    language_code :: Maybe Text -- A two-letter ISO 639-1 language code.
  } deriving (Show, Eq, Generic, ToJSON)

-- | Default methods for Requests
createGetUpdates :: BotSynonyms.UpdateId -> RequestOptions
createGetUpdates updateId = GetUpdates {
  offset = Just updateId,
  limit = Nothing,
  timeout = Just Settings.timeout,
  allowed_updates = Just ["message"]
}

createTextMessage :: BotSynonyms.ChatId -> Text -> RequestOptions
createTextMessage chatId messageText = SendMessage {
  chat_id = chatId,
  text = messageText,
  disable_notification = Nothing,
  reply_to_message_id = Nothing
}

createEchoMessage :: BotSynonyms.ChatId -> BotSynonyms.MessageId -> RequestOptions
createEchoMessage chatId messageId = CopyMessage { 
  chat_id = chatId,
  from_chat_id = chatId,
  message_id = messageId
}

createKeyboardMessage :: BotSynonyms.ChatId -> Text -> TeleKeyboard.Keyboard -> RequestOptions
createKeyboardMessage chatId question markupIn = QueryMessage {
  chat_id = chatId,
  text = question,
  disable_notification = Nothing,
  reply_to_message_id = Nothing,
  reply_markup = Just markupIn
}

createCommands :: RequestOptions
createCommands = SetCommands {
  commands = [TeleCommand.helpCommand, TeleCommand.repCommand],
  scope = Just TeleCommandScope.defaultCommandScope,
  language_code = Just ""
}
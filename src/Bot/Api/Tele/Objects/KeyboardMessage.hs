{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Tele.Objects.KeyboardMessage where

import Data.Aeson.Types (ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Bot.Api.Tele.Objects.Keyboard as TeleKeyboard
import qualified Bot.Objects.Synonyms as BotSynonyms

data KeyboardMessage = KeyboardMessage
  { chat_id :: BotSynonyms.ChatId,
    text :: Text,
    disable_notification :: Maybe Bool,
    reply_to_message_id :: Maybe BotSynonyms.MessageId,
    reply_markup :: Maybe TeleKeyboard.Keyboard
  }
  deriving (Show, Eq, Generic, ToJSON)

createKeyboardMessage ::
  BotSynonyms.ChatId ->
  Text ->
  TeleKeyboard.Keyboard ->
  KeyboardMessage
createKeyboardMessage chatId question markupIn =
  KeyboardMessage
    { chat_id = chatId,
      text = question,
      disable_notification = Nothing,
      reply_to_message_id = Nothing,
      reply_markup = Just markupIn
    }

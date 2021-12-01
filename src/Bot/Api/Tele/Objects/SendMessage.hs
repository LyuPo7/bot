{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Tele.Objects.SendMessage where

import Data.Aeson.Types (ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

data SendMessage = SendMessage
  { chat_id :: BotSynonyms.ChatId,
    text :: Text,
    disable_notification :: Maybe Bool,
    reply_to_message_id :: Maybe BotSynonyms.MessageId
  }
  deriving (Show, Eq, Generic, ToJSON)

createTextMessage ::
  BotSynonyms.ChatId ->
  Text ->
  SendMessage
createTextMessage chatId messageText =
  SendMessage
    { chat_id = chatId,
      text = messageText,
      disable_notification = Nothing,
      reply_to_message_id = Nothing
    }

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Tele.Objects.Message where

import Data.Aeson.Types (FromJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Bot.Api.Tele.Objects.Chat as TeleChat
import qualified Bot.Api.Tele.Objects.MessageEntity as TeleMessageEntity
import qualified Bot.Objects.Synonyms as BotSynonyms

data Message = Message
  { message_id :: BotSynonyms.MessageId,
    chat :: TeleChat.Chat,
    text :: Maybe Text,
    entities :: Maybe [TeleMessageEntity.MessageEntity]
  }
  deriving (Show, Read, Eq, Generic, FromJSON)

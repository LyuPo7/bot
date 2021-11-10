{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.Message where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Geo (Geo(..))
import Bot.Vk.Parser.Objects.Attachment (Attachment(..))
import Bot.Vk.Parser.Objects.Synonyms (MessageId, UserId)

data Message = Message {
  message_id :: Maybe MessageId,
  user_id :: UserId,
  body :: Text,
  geo :: Maybe Geo, 
  attachments :: Maybe [Attachment],
  fwd_messages :: Maybe [Message]
} deriving (Show, Read, Eq, Generic, FromJSON)
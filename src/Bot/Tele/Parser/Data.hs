{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Bot.Tele.Parser.Data where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (camelTo2)
import Data.Aeson.Types (ToJSON(..), FromJSON(..), genericToJSON, defaultOptions, fieldLabelModifier, genericParseJSON)

-- Synonims
type Status = Bool -- Responce status.
type ChatID = Integer
type UpdateID = Integer
type MessageID = Integer
type RepNum = Integer
type Mode = Text

-- | Chat (only private chats)
data Chat = Chat {
  chat_id :: ChatID -- Unique identifier for this chat.
  } deriving (Show, Read, Eq, Generic) 

instance FromJSON Chat where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON Chat where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | Message
data Message = Message {
  message_messageId :: MessageID, -- Unique message identifier inside this chat.
  message_chat :: Chat, -- Conversation the message belongs to.
  message_text :: Maybe Text, -- For text messages, the actual UTF-8 text of the message, 0-4096 characters.
  message_entities :: Maybe [MessageEntity] --  For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text.
  } deriving (Show, Read, Eq, Generic)

instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

instance ToJSON Message where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | MessageEntity
data MessageEntity = MessageEntity {
  messageent_type :: Text -- Type of the entity. Can be “mention” (@username), “hashtag” (#hashtag),...
  } deriving (Show, Read, Eq, Generic)

instance FromJSON MessageEntity where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

instance ToJSON MessageEntity where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 11 }

-- | Update
data Update = Update {
  update_updateId :: UpdateID, -- The update's unique identifier.
  update_message :: Maybe Message -- New incoming message of any kind — text, photo, sticker, etc.
} deriving (Show, Read, Eq, Generic)

instance FromJSON Update where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7 }

instance ToJSON Update where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7 }

-- | UpdateData
data UpdateData = UpdateData {
  ok :: Status, -- Responce status.
  result :: [Update] -- Update's array.
} deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
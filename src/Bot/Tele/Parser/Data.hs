{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Tele.Parser.Data where

import qualified Data.Aeson.Types as A
import Data.Aeson.Types (ToJSON(..), FromJSON(..))
import Data.Aeson (camelTo2)
import Data.Text (Text)
import GHC.Generics (Generic)

-- Synonyms
type Status = Bool -- Response status.
type ChatID = Integer
type UpdateID = Integer
type MessageID = Integer
type RepNum = Integer
type Mode = Text

-- | Chat (only private chats)
newtype Chat = Chat {
  chat_id :: ChatID -- Unique identifier for this chat.
  } deriving (Show, Read, Eq, Generic) 

instance FromJSON Chat where
  parseJSON = A.genericParseJSON A.defaultOptions {
    A.fieldLabelModifier = camelTo2 '_' . drop 5 }

instance ToJSON Chat where
  toJSON = A.genericToJSON A.defaultOptions {
    A.fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | Message
data Message = Message {
  message_messageId :: MessageID, -- Unique message identifier..
  message_chat :: Chat, -- Conversation the message belongs to.
  message_text :: Maybe Text, -- Text of message.
  message_entities :: Maybe [MessageEntity] -- Entities of message.
  } deriving (Show, Read, Eq, Generic)

instance FromJSON Message where
  parseJSON = A.genericParseJSON A.defaultOptions {
    A.fieldLabelModifier = camelTo2 '_' . drop 8 }

instance ToJSON Message where
  toJSON = A.genericToJSON A.defaultOptions {
    A.fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | MessageEntity
newtype MessageEntity = MessageEntity {
  messageent_type :: Text -- Type of the entity.
  } deriving (Show, Read, Eq, Generic)

instance FromJSON MessageEntity where
  parseJSON = A.genericParseJSON A.defaultOptions {
    A.fieldLabelModifier = camelTo2 '_' . drop 11 }

instance ToJSON MessageEntity where
  toJSON = A.genericToJSON A.defaultOptions {
    A.fieldLabelModifier = camelTo2 '_' . drop 11 }

-- | Update
data Update = Update {
  update_updateId :: UpdateID, -- The update's unique identifier.
  update_message :: Maybe Message -- New incoming message of any kind.
} deriving (Show, Read, Eq, Generic)

instance FromJSON Update where
  parseJSON = A.genericParseJSON A.defaultOptions {
    A.fieldLabelModifier = camelTo2 '_' . drop 7 }

instance ToJSON Update where
  toJSON = A.genericToJSON A.defaultOptions {
    A.fieldLabelModifier = camelTo2 '_' . drop 7 }

-- | UpdateData
data UpdateData = UpdateData {
  ok :: Status, -- Response status.
  result :: [Update] -- Update's array.
} deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Tele.Parser.Objects.Message where

import Data.Aeson.Types (FromJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Bot.Tele.Parser.Objects.Synonyms (MessageId)
import Bot.Tele.Parser.Objects.Chat (Chat(..))
import Bot.Tele.Parser.Objects.MessageEntity (MessageEntity(..))

data Message = Message {
  message_id :: MessageId,
  chat :: Chat,
  text :: Maybe Text,
  entities :: Maybe [MessageEntity]
  } deriving (Show, Read, Eq, Generic, FromJSON)
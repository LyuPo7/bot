{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Tele.Parser.Objects.Chat where

import Data.Aeson.Types (FromJSON(..))
import GHC.Generics (Generic)

import Bot.Tele.Parser.Objects.Synonyms (ChatId)

newtype Chat = Chat {
  id :: ChatId -- only private chats
  } deriving (Show, Read, Eq, Generic, FromJSON)
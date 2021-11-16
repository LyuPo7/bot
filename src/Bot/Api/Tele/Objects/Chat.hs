{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Tele.Objects.Chat where

import Data.Aeson.Types (FromJSON(..))
import GHC.Generics (Generic)

import Bot.Objects.Synonyms as BotSynonyms

newtype Chat = Chat {
  id :: BotSynonyms.ChatId -- only private chats
} deriving (Show, Read, Eq, Generic, FromJSON)
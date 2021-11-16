{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.Sticker where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import qualified Bot.Objects.Synonyms as BotSynonyms

newtype Sticker = Sticker {
  id :: BotSynonyms.StickerId
} deriving (Show, Read, Eq, Generic, FromJSON)
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.Gift where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import qualified Bot.Objects.Synonyms as BotSynonyms

newtype Gift = Gift {
  id :: BotSynonyms.GiftId
} deriving (Show, Read, Eq, Generic, FromJSON)
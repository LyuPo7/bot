{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.Link where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import qualified Bot.Objects.Synonyms as BotSynonyms

newtype Link = Link {
  url :: BotSynonyms.Url
} deriving (Show, Read, Eq, Generic, FromJSON)
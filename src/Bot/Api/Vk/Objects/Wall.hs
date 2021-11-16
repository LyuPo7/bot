{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.Wall where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import qualified Bot.Objects.Synonyms as BotSynonyms

data Wall = Wall {
  id :: BotSynonyms.WallId,
  owner_id :: BotSynonyms.UserId
} deriving (Show, Read, Eq, Generic, FromJSON)
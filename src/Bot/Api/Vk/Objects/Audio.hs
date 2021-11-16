{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.Audio where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import qualified Bot.Objects.Synonyms as BotSynonyms

data Audio = Audio {
  id :: BotSynonyms.AudioId,
  owner_id :: BotSynonyms.UserId
} deriving (Show, Read, Eq, Generic, FromJSON)
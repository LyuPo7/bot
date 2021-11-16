{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.Video where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import qualified Bot.Objects.Synonyms as BotSynonyms

data Video = Video {
  id :: BotSynonyms.VideoId,
  owner_id :: BotSynonyms.UserId,
  access_key :: BotSynonyms.AccessKey 
} deriving (Show, Read, Eq, Generic, FromJSON)
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.Video where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

data Video = Video
  { id :: BotSynonyms.VideoId,
    owner_id :: BotSynonyms.UserId,
    access_key :: BotSynonyms.AccessKey
  }
  deriving (Show, Read, Eq, Generic, FromJSON)

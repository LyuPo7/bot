{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.Video where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Synonyms (VideoId, UserId, AccessKey)

data Video = Video {
  id :: VideoId,
  owner_id :: UserId,
  access_key :: AccessKey 
} deriving (Show, Read, Eq, Generic, FromJSON)
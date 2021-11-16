{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.Photo where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import qualified Bot.Objects.Synonyms as BotSynonyms

data Photo = Photo {
  id :: BotSynonyms.PhotoId,
  owner_id :: BotSynonyms.UserId,
  access_key :: BotSynonyms.AccessKey
} deriving (Show, Read, Eq, Generic, FromJSON)
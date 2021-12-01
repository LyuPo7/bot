{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.Photo where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

data Photo = Photo
  { id :: BotSynonyms.PhotoId,
    owner_id :: BotSynonyms.UserId,
    access_key :: BotSynonyms.AccessKey
  }
  deriving (Show, Read, Eq, Generic, FromJSON)

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.Photo where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Synonyms (PhotoId, UserId, AccessKey)

data Photo = Photo {
  id :: PhotoId,
  owner_id :: UserId,
  access_key :: AccessKey
} deriving (Show, Read, Eq, Generic, FromJSON)
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.UploadObject where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Synonyms (UserId, ObjectId, Url)

data UploadObject = UploadObject {
  id :: ObjectId,
  owner_id :: UserId,
  url :: Url
} deriving (Show, Read, Eq, Generic, FromJSON)
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.UploadObject where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import qualified Bot.Objects.Synonyms as BotSynonyms

data UploadObject = UploadObject {
  id :: BotSynonyms.ObjectId,
  owner_id :: BotSynonyms.UserId,
  url :: BotSynonyms.Url
} deriving (Show, Read, Eq, Generic, FromJSON)
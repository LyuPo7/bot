{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.UploadUrl where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import qualified Bot.Objects.Synonyms as BotSynonyms

newtype UploadUrl = UploadUrl {
  upload_url :: BotSynonyms.Url
} deriving (Show, Read, Eq, Generic, FromJSON)
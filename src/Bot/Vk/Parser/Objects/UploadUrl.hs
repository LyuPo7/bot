{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.UploadUrl where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Synonyms (Url)

-- | UploadUrl
newtype UploadUrl = UploadUrl {
  upload_url :: Url
} deriving (Show, Read, Eq, Generic, FromJSON)
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.UploadUrlResponse where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.UploadUrl (UploadUrl(..))

-- | UploadUrlResponse
newtype UploadUrlResponse = UploadUrlResponse {
  response :: Maybe UploadUrl
} deriving (Show, Read, Eq, Generic, FromJSON) 
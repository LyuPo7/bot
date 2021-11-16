{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.UploadUrlResponse where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import qualified Bot.Api.Vk.Objects.UploadUrl as VkUpUrl

newtype UploadUrlResponse = UploadUrlResponse {
  response :: Maybe VkUpUrl.UploadUrl
} deriving (Show, Read, Eq, Generic, FromJSON) 
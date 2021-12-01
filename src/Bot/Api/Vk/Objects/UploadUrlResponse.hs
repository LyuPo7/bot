{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.UploadUrlResponse where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Api.Vk.Objects.UploadUrl as VkUpUrl

newtype UploadUrlResponse = UploadUrlResponse
  { response :: Maybe VkUpUrl.UploadUrl
  }
  deriving (Show, Read, Eq, Generic, FromJSON)

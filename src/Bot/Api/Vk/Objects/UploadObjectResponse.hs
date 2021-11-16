{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.UploadObjectResponse where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Api.Vk.Objects.UploadObject as VkUpObj

newtype UploadObjectResponse = UploadObjectResponse {
  response :: [VkUpObj.UploadObject]
} deriving (Show, Read, Eq, Generic, FromJSON)
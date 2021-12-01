{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.UploadObjectResponse where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import Bot.Api.Vk.Objects.UploadObject as VkUpObj

newtype UploadObjectResponse = UploadObjectResponse
  { response :: [VkUpObj.UploadObject]
  }
  deriving (Show, Read, Eq, Generic, FromJSON)

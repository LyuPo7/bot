{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.UploadFileResponse where

import Data.Aeson.Types (FromJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

newtype UploadFileResponse = UploadFileResponse
  { file :: Maybe Text
  }
  deriving (Show, Read, Eq, Generic, FromJSON)

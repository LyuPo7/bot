{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.UploadFileResponse where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

newtype UploadFileResponse = UploadFileResponse {
  file :: Maybe Text
} deriving (Show, Read, Eq, Generic, FromJSON)
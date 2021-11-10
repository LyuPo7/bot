{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.UploadFileResponse where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

newtype UploadFileResponse = UploadFileResponse {
  file :: Maybe Text
} deriving (Show, Read, Eq, Generic, FromJSON)
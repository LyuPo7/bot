{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.UploadObjectResponse where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.UploadObject (UploadObject(..))

newtype UploadObjectResponse = UploadObjectResponse {
  response :: [UploadObject]
} deriving (Show, Read, Eq, Generic, FromJSON)
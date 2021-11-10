{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.Document where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Synonyms (DocumentId, UserId, Url, AccessKey)

data Document = Document {
  id :: DocumentId,
  owner_id :: UserId,
  title :: Text,
  url :: Url,
  access_key :: AccessKey
} deriving (Show, Read, Eq, Generic, FromJSON)
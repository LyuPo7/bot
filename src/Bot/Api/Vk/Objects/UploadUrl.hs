{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.UploadUrl where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

newtype UploadUrl = UploadUrl
  { upload_url :: BotSynonyms.Url
  }
  deriving (Show, Read, Eq, Generic, FromJSON)

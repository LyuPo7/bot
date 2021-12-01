{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.Document where

import Data.Aeson.Types (FromJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

data Document = Document
  { id :: BotSynonyms.DocId,
    owner_id :: BotSynonyms.UserId,
    title :: Text,
    url :: BotSynonyms.Url,
    access_key :: BotSynonyms.AccessKey
  }
  deriving (Show, Read, Eq, Generic, FromJSON)

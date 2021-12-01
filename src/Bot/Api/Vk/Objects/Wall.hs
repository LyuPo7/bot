{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.Wall where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

data Wall = Wall
  { id :: BotSynonyms.WallId,
    owner_id :: BotSynonyms.UserId
  }
  deriving (Show, Read, Eq, Generic, FromJSON)

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.Audio where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

data Audio = Audio
  { id :: BotSynonyms.AudioId,
    owner_id :: BotSynonyms.UserId
  }
  deriving (Show, Read, Eq, Generic, FromJSON)

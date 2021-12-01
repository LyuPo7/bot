{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.WallReply where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

data WallReply = WallReply
  { id :: BotSynonyms.WallReplyId,
    owner_id :: BotSynonyms.UserId
  }
  deriving (Show, Read, Eq, Generic, FromJSON)

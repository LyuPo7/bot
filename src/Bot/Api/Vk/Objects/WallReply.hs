{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.WallReply where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import qualified Bot.Objects.Synonyms as BotSynonyms

data WallReply = WallReply {
  id :: BotSynonyms.WallReplyId,
  owner_id :: BotSynonyms.UserId
} deriving (Show, Read, Eq, Generic, FromJSON)
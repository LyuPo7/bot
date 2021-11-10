{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.WallReply where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Synonyms (WallReplyId, UserId)

data WallReply = WallReply {
  id :: WallReplyId,
  owner_id :: UserId
} deriving (Show, Read, Eq, Generic, FromJSON)
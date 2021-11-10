{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.PollResponse where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Server (ServerText(..))

newtype PollResponse = PollResponse {
  response :: ServerText
} deriving (Show, Read, Eq, Generic, FromJSON)
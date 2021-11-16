{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.PollResponse where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import qualified Bot.Api.Vk.Objects.Server as VkServer

newtype PollResponse = PollResponse {
  response :: VkServer.ServerText
} deriving (Show, Read, Eq, Generic, FromJSON)
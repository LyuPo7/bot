{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.PollResponse where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import qualified Bot.Api.Vk.Objects.Server as VkServer

newtype PollResponse = PollResponse
  { response :: VkServer.ServerText
  }
  deriving (Show, Read, Eq, Generic, FromJSON)

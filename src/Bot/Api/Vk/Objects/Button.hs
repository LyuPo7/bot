{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.Button where

import Data.Aeson.Types (FromJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Bot.Api.Vk.Objects.Action as VkAction

data Button = Button
  { action :: VkAction.Action,
    color :: Maybe Text
  }
  deriving (Show, Read, Eq, Generic, FromJSON)

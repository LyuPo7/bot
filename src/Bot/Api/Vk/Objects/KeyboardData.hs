{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.KeyboardData where

import Data.Aeson.Types (FromJSON (..))
import GHC.Generics (Generic)

import Bot.Api.Vk.Objects.Keyboard as VkKeyboard

newtype KeyboardData = KeyboardData
  { keyboard :: Maybe VkKeyboard.Keyboard
  }
  deriving (Show, Read, Eq, Generic, FromJSON)

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.KeyboardData where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Api.Vk.Objects.Keyboard as VkKeyboard

newtype KeyboardData = KeyboardData {
  keyboard :: Maybe VkKeyboard.Keyboard
} deriving (Show, Read, Eq, Generic, FromJSON)
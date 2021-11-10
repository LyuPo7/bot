{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.KeyboardData where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Keyboard (Keyboard(..))

newtype KeyboardData = KeyboardData {
  keyboard :: Maybe Keyboard
} deriving (Show, Read, Eq, Generic, FromJSON)
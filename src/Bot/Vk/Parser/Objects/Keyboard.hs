{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.Keyboard where

import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))
  
import Bot.Vk.Parser.Objects.Button (Button(..))

data Keyboard = Keyboard {
  one_ime :: Maybe Bool, -- Hides the keyboard after the initial use.
  buttons :: [[Button]],
  inline :: Maybe Bool -- Shows the keyboard inside the message.
} deriving (Show, Read, Eq, Generic, FromJSON)
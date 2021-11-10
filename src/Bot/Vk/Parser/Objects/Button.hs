{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.Button where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Action (Action(..))

data Button = Button {
  action :: Action,
  color :: Maybe Text
} deriving (Show, Read, Eq, Generic, FromJSON)
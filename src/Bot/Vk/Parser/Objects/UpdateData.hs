{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Parser.Objects.UpdateData where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

import Bot.Vk.Parser.Objects.Update (Update(..))

data UpdateData = UpdateData {
  ts :: Text,
  updates :: [Update]
} deriving (Show, Read, Eq, Generic, FromJSON)
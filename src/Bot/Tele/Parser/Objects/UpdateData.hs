{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Tele.Parser.Objects.UpdateData where

import Data.Aeson.Types (FromJSON(..))
import GHC.Generics (Generic)

import Bot.Tele.Parser.Objects.Synonyms (Status)
import Bot.Tele.Parser.Objects.Update (Update(..))

data UpdateData = UpdateData {
  ok :: Status,
  result :: [Update]
} deriving (Show, Read, Eq, Generic, FromJSON)
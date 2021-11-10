{-# LANGUAGE DeriveGeneric #-}

module Bot.Tele.Parser.Objects.MessageEntity where

import qualified Data.Aeson.Types as A
import Data.Aeson.Types (FromJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)

newtype MessageEntity = MessageEntity {
  entity_type :: Text
  } deriving (Show, Read, Eq, Generic)

instance FromJSON MessageEntity where
  parseJSON = A.genericParseJSON A.defaultOptions {
    A.fieldLabelModifier = drop 7 }
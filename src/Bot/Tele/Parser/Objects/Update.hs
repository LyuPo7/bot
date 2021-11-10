{-# LANGUAGE DeriveGeneric #-}

module Bot.Tele.Parser.Objects.Update where

import qualified Data.Aeson as A
import Data.Aeson.Types (FromJSON(..))
import GHC.Generics (Generic)

import Bot.Tele.Parser.Objects.Synonyms (UpdateId)
import Bot.Tele.Parser.Objects.Message (Message(..))

data Update = Update {
  id :: UpdateId,
  message :: Maybe Message
} deriving (Show, Read, Eq, Generic)

instance FromJSON Update where
  parseJSON = A.withObject "Action" $ \o ->
    Update
      <$> o A..: "update_id"
      <*> o A..: "message"
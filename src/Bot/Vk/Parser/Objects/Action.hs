{-# LANGUAGE DeriveGeneric #-}

module Bot.Vk.Parser.Objects.Action where

import qualified Data.Aeson as A
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..))

data Action = Action {
  action_type :: Text, -- "callback", "text", ... .
  label :: Text, -- Button text.
  payload :: Text -- Additional information.
} deriving (Show, Read, Eq,Generic)

instance FromJSON Action where
  parseJSON = A.withObject "Action" $ \o ->
    Action
      <$> o A..: "type"
      <*> o A..: "label"
      <*> o A..: "payload"
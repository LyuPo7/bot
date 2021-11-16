{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.Update where

import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import Data.Aeson.Types (FromJSON(..))

import qualified Bot.Api.Vk.Objects.Message as VkMessage

data Update = Update {
  update_type :: Text, -- "new_message", "reply_message", ... .
  object :: VkMessage.Message
} deriving (Show, Read, Eq, Generic)

instance FromJSON Update where
  parseJSON = A.withObject "Update" $ \o ->
    Update
      <$> o A..: "type"
      <*> o A..: "object"
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.Server where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..), defaultOptions,
                         fieldLabelModifier, genericParseJSON)

-- | Server
data Server = Server {
  key :: Text, -- Key for requests.
  server :: Text, -- Requests server.
  ts :: Integer  -- Server time stamp.
} deriving (Show, Read, Eq, Generic, FromJSON)

-- | ServerText with ts String
data ServerText = ServerText {
  text_key :: Text, -- Key for requests.
  text_server :: Text, -- Requests server.
  text_ts :: Text  -- Server time stamp.
} deriving (Show, Read, Eq, Generic) 

instance FromJSON ServerText where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 5 }
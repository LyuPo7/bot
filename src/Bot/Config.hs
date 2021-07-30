{-# LANGUAGE DeriveGeneric #-}

module Bot.Config where

import qualified Data.ByteString.Lazy as B

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (camelTo2, eitherDecode)
import Data.Aeson.Types (ToJSON(..), FromJSON(..), genericToJSON, defaultOptions, fieldLabelModifier, genericParseJSON)

-- | Bot Config
data Config = Config {
    botApi :: Text,
    botToken :: Text,
    botInitialReplyNumber :: Integer,
    botQuestion :: Text,
    botDescription :: Text,
    botGroupId :: Maybe Integer
} deriving (Show,Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' }

instance ToJSON Config where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' }

-- | Config parser
parseConfig :: IO B.ByteString -> IO Config
parseConfig config = do
  -- Get JSON data and decode it
  d <- (eitherDecode <$> config) :: IO (Either String Config)
  case d of
    Left err -> do
      fail err --main = toTry `catchIOError` handler
    Right ps -> do
      --logDebug logh "Poll response was successfully parsed."
      return ps
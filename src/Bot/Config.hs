{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Bot.Config where

import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as A
import qualified Control.Exception as Exc
import GHC.Generics (Generic)

import qualified Bot.Exception as E
import qualified Bot.Settings as Settings
import qualified Bot.Logger as Logger

-- | General Bot Config
data Config = Config {
  cSettings :: Settings.Config,
  cLogger :: Logger.Config
} deriving (Show, Generic, Eq)

instance A.FromJSON Config where
    parseJSON = A.withObject "General Config" $ \o ->
        Config
            <$> o A..: "api_settings"
            <*> o A..: "logger_settings"

-- | Get settings from config
getConfig :: IO Config
getConfig = do
  conf <- readConfig
  let config = parseConfig conf >>= checkConfig
  case config of
    Right cnfg -> return cnfg
    Left err -> Exc.throwIO err

checkConfig :: Config -> Either E.BotError Config
checkConfig config 
  | Settings.botApi cSet `notElem` ["vk", "telegram"] = Left $ E.ParseConfigError "Incorrect field 'bot_api' in config.json"
  | Settings.botInitialReplyNumber cSet < 0 = Left $ E.ParseConfigError "Incorrect field in config.json: 'bot_initial_reply_number' < 0"
  | Settings.botInitialReplyNumber cSet > 5 = Left $ E.ParseConfigError "Incorrect field in config.json: 'bot_initial_reply_number' > 5"
  | Logger.cVerbocity cLog `notElem` [Just Logger.Debug, Just Logger.Info, Just Logger.Warning, Just Logger.Error, Nothing] = Left $ E.ParseConfigError "Incorrect field 'verbocity' in config.json"
  | otherwise = Right config where
      cLog = cLogger config
      cSet = cSettings config

-- | Read the config JSON file.
readConfig :: IO B.ByteString
readConfig = B.readFile Settings.configFile
      
-- | Config parser
parseConfig :: B.ByteString -> Either E.BotError Config
parseConfig config = do
  let d = A.eitherDecode config :: Either String Config
  case d of
    Left err -> Left $ E.ParseConfigError err
    Right ps -> Right ps
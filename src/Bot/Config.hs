{-# LANGUAGE DeriveGeneric #-}

module Bot.Config where

import qualified Control.Exception as Exc
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)

import qualified Bot.Exception as E
import qualified Bot.Logger.Logger as Logger
import qualified Bot.Settings as Settings

data Config = Config
  { cSettings :: Settings.Config,
    cLogger :: Logger.Config
  }
  deriving (Show, Generic, Eq)

instance A.FromJSON Config where
  parseJSON = A.withObject "General Config" $ \o ->
    Config
      <$> o A..: "api_settings"
      <*> o A..: "logger_settings"

getConfig :: IO Config
getConfig = do
  conf <- readConfig
  let config = parseConfig conf >>= checkConfig
  case config of
    Right cnfg -> return cnfg
    Left err -> Exc.throwIO err

checkConfig ::
  Config ->
  Either E.BotError Config
checkConfig config
  | Settings.botInitialReplyNumber cSet < 0 =
    Left $
      E.ParseConfigError
        "Incorrect field in config.json: \
        \'bot_initial_reply_number' < 0"
  | Settings.botInitialReplyNumber cSet > 5 =
    Left $
      E.ParseConfigError
        "Incorrect field in config.json: \
        \'bot_initial_reply_number' > 5"
  | Logger.cVerbosity cLog
      `notElem` [ Just Logger.Debug,
                  Just Logger.Info,
                  Just Logger.Warning,
                  Just Logger.Error,
                  Nothing
                ] =
    Left $
      E.ParseConfigError
        "Incorrect field \
        \'verbosity' in config.json"
  | otherwise = Right config
 where
  cLog = cLogger config
  cSet = cSettings config

readConfig :: IO B.ByteString
readConfig = B.readFile Settings.configFile

parseConfig ::
  B.ByteString ->
  Either E.BotError Config
parseConfig config = do
  let d = A.eitherDecode config
  case d of
    Left err -> Left $ E.ParseConfigError err
    Right ps -> Right ps

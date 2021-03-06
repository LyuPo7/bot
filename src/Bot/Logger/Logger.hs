{-# LANGUAGE DeriveGeneric #-}

module Bot.Logger.Logger where

import Control.Monad (mzero, when)
import qualified Data.Aeson as A
import Data.Aeson.Types (FromJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import TextShow (TextShow, showb, showt)
import Prelude hiding (log)

import Data.Time (defaultTimeLocale, formatTime, getZonedTime)

data Handle m = Handle
  { log :: LogMessage -> Text -> m (),
    hConfig :: Config
  }

newtype Config = Config
  { cVerbosity :: Maybe Level
  }
  deriving (Show, Generic, Eq)

instance A.FromJSON Config where
  parseJSON = A.withObject "General Config" $ \o ->
    Config <$> o A..:? "verbosity"

withHandleIO ::
  Config ->
  (Handle IO -> IO a) ->
  IO a
withHandleIO config f = f $ newHandleIO config

newHandleIO ::
  Config ->
  Handle IO
newHandleIO config = do
  let globalLevel = fromMaybe Debug $ cVerbosity config
  Handle
    { hConfig = config,
      log = \logMes str ->
        when (level logMes >= globalLevel) $ do
          let levelMes = level logMes
          currentTime <- getTime
          putStrLn $
            T.unpack
              ( ( renderColor levelMes
                    <> showt levelMes
                    <> resetColor
                )
                  <> " | "
                  <> currentTime
                  <> " | "
                  <> str
              )
    }

data Level
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Enum, Bounded, Read, Generic)

instance Show Level where
  show Debug = "[DEBUG]"
  show Info = "[INFO] "
  show Warning = "[WARN] "
  show Error = "[ERROR]"

instance TextShow Level where
  showb Debug = "[DEBUG]"
  showb Info = "[INFO] "
  showb Warning = "[WARN] "
  showb Error = "[ERROR]"

instance FromJSON Level where
  parseJSON = A.withText "Level Logger" $
    \case
      "debug" -> pure Debug
      "info" -> pure Info
      "warning" -> pure Warning
      "error" -> pure Error
      _ -> mzero

newtype LogMessage = LogMessage
  { level :: Level
  }

logDebug,
  logInfo,
  logWarning,
  logError ::
    Handle m ->
    Text ->
    m ()
logDebug = (`log` LogMessage {level = Debug})
logInfo = (`log` LogMessage {level = Info})
logWarning = (`log` LogMessage {level = Warning})
logError = (`log` LogMessage {level = Error})

getTime :: IO Text
getTime = do
  T.pack . formatTime defaultTimeLocale (T.unpack defaultTimeFormat)
    <$> getZonedTime

defaultTimeFormat :: Text
defaultTimeFormat = "%_Y-%m-%d %T.%3q"

resetColor :: Text
resetColor = normalCS

renderColor ::
  Level ->
  Text
renderColor lev = case lev of
  Debug -> purpleCS
  Info -> blueCS
  Warning -> yellowCS
  Error -> redCS

normalCS, redCS, purpleCS, blueCS, yellowCS :: Text
normalCS = "\o33[0;0m"
redCS = "\o33[1;31m"
purpleCS = "\o33[0;35m"
blueCS = "\o33[0;34m"
yellowCS = "\o33[1;33m"

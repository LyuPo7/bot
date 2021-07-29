{-# LANGUAGE DeriveGeneric #-}

module Logger where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, getZonedTime)
import GHC.Generics (Generic)
import Prelude hiding (log)

data Level
  = Debug -- ^ Debug messages
  | Info -- ^ Information
  | Warning -- ^ General Warnings
  | Error -- ^ General Errors
  deriving (Eq, Ord, Enum, Bounded, Read, Generic)

data LogMessage =
  LogMessage
    { level :: Level
    , time :: IO String
    }

instance Show Level where
  show Debug = "[DEBUG]"
  show Info = "[INFO] "
  show Warning = "[WARN] "
  show Error = "[ERROR]"

newtype Handle = Handle {
  log :: LogMessage -> String -> IO ()
}

logDebug, logInfo, logWarning, logError :: Handle -> String -> IO ()
logDebug = (`log` LogMessage {level = Debug, time = getTime})

logInfo = (`log` LogMessage {level = Info, time = getTime})

logWarning = (`log` LogMessage {level = Warning, time = getTime})

logError = (`log` LogMessage {level = Error, time = getTime})

getTime :: IO String
getTime = do
  formatTime defaultTimeLocale defaultTimeFormat <$> getZonedTime

defaultTimeFormat :: String
defaultTimeFormat = "%_Y-%m-%d %T.%3q"

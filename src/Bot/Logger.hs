{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Bot.Logger where

import Prelude hiding (log)
import qualified Data.Text as T
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (FromJSON)
import TextShow (TextShow, showb, showt)
import Control.Monad (when, mzero)

import Data.Time (defaultTimeLocale, formatTime, getZonedTime)

-- | Types
data Level
  = Debug -- Debug messages
  | Info -- Info message
  | Warning -- Warning message
  | Error -- Error message
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
  parseJSON = A.withText "Config Logger" $ \val ->
    case val of
      "debug" -> pure Debug
      "info" -> pure Info
      "warning" -> pure Warning
      "error" -> pure Error
      _ -> mzero

data Config = Config {
  cVerbocity :: Maybe Level
} deriving (Show, Generic, Eq)

instance A.FromJSON Config where
    parseJSON = A.withObject "General Config" $ \o ->
        Config <$> o A..:? "verbocity"

data LogMessage = LogMessage { 
  level :: Level
}

data Handle m = Handle {
  log :: LogMessage -> Text -> m (),
  hconfig :: Config
}

withHandleIO :: Config -> (Handle IO -> IO a) -> IO a
withHandleIO config f = f $ newHandleIO config

{-- | create Handle IO --}
newHandleIO :: Config -> Handle IO
newHandleIO config = do
  let globalLevel = fromMaybe Debug $ cVerbocity config
  Handle {
    hconfig = config,
    log = \logMes str -> 
      when (level logMes >= globalLevel) $ do
        let levelMes = level logMes
        currentTime <- getTime
        putStrLn $ T.unpack ((renderColor levelMes <> showt levelMes <> resetColor) <> " | " <> currentTime <> " | " <> str)
  }
  
logDebug, logInfo, logWarning, logError :: Handle m -> Text -> m ()
logDebug = (`log` LogMessage {level = Debug})
logInfo = (`log` LogMessage {level = Info})
logWarning = (`log` LogMessage {level = Warning})
logError = (`log` LogMessage {level = Error})

getTime :: IO Text
getTime = do 
  T.pack <$> formatTime defaultTimeLocale (T.unpack defaultTimeFormat) <$> getZonedTime

defaultTimeFormat :: Text
defaultTimeFormat = "%_Y-%m-%d %T.%3q"

resetColor :: Text
resetColor = normalCS

renderColor :: Level -> Text
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
{- color schemes
CLR="\[\033[0;0m\]"   # normal color scheme
BK="\[\O33[0;30m\]"   # black
BL="\[\033[0;34m\]"   # blue
GR="\[\033[0;32m\]"   # green
CY="\[\033[0;36m\]"   # cyan
RD="\[\033[0;31m\]"   # red
PL="\[\033[0;35m\]"   # purple
BR="\[\033[0;33m\]"   # brown
GY="\[\033[1;30m\]"   # grey
#eGY="\[\033[0;37m\]"  # light gray
#eBL="\[\033[1;34m\]"  # light blue
#eGR="\[\033[1;32m\]"  # light green
#eCY="\[\033[1;36m\]"  # light cyan
#eRD="\[\033[1;31m\]"  # light red
#ePL="\[\033[1;35m\]"  # light purple
#eYW="\[\033[1;33m\]"  # yellow
#eWT="\[\033[1;37m\]"  # white
-}
module LoggerIO where

import Data.Maybe (fromMaybe)
import Data.Time (getZonedTime,getCurrentTime,formatTime,defaultTimeLocale)
import Control.Monad (when)

import qualified Logger as L

newHandle :: IO L.Handle
newHandle = do
    let globalLevel = L.Debug
    return L.Handle {
        L.log = \logMes str -> when (L.level logMes >= globalLevel) $ do
            let levelMes = L.level logMes
            currentTime <- L.time logMes
            putStrLn $ (renderColor levelMes <> show levelMes <> resetColor) ++ " | " ++ currentTime ++ " | " ++ str
    }

resetColor = normalCS
renderColor level = case level of
                  L.Debug -> purpleCS
                  L.Info -> blueCS
                  L.Warning -> yellowCS
                  L.Error -> redCS

normalCS, redCS, purpleCS, blueCS, yellowCS :: String
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
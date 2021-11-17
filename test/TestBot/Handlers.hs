module TestBot.Handlers where

import qualified Bot.Logger.Logger as Logger

logH :: Logger.Handle Maybe
logH = Logger.Handle {
  Logger.log = \_ _ -> return (),
  Logger.hConfig = Logger.Config {Logger.cVerbosity = Nothing}
}
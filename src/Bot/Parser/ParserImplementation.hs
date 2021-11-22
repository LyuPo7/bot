module Bot.Parser.ParserImplementation where

import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Settings as Settings
import qualified Bot.Logger.Logger as Logger
import qualified Bot.System.System as BotSystem

withHandleIO :: Logger.Handle IO -> BotSystem.Handle IO ->
                Settings.Config -> (BotParser.Handle IO -> IO a) -> IO a
withHandleIO logger sysH cParser f = do
  let handle = BotParser.Handle logger sysH cParser
  f handle
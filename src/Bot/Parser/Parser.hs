module Bot.Parser.Parser where

import qualified Bot.Logger.Logger as Logger
import qualified Bot.System.System as BotSystem
import qualified Bot.Settings as Settings

data Handle m = Handle {
  hLogger :: Logger.Handle m,
  hSystem :: BotSystem.Handle m,
  cParser :: Settings.Config
}

module Bot.Tele.Parser.ParserSpec where

import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings

data Handle m = Handle {
  hLogger :: Logger.Handle m,
  cPar :: Settings.Config
}
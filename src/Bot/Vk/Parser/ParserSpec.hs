module Bot.Vk.Parser.ParserSpec where

import qualified Bot.Logger as Logger

newtype Handle m = Handle {
  hLogger :: Logger.Handle m
}
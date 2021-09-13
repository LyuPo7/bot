module Bot.Vk.Request.RequestsSpec where

import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Vk.Parser.ParserSpec as Parser

data Handle m = Handle {
    hLogger :: Logger.Handle m,
    configReq :: Settings.Config,
    hParser :: Parser.Handle m
}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Tele.Parser.ParserSpec where

import qualified Bot.Logger as Logger

data Handle m = Handle {
    hLogger :: Logger.Handle m
}
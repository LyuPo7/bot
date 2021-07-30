{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import qualified Bot.Config as Config
import qualified Bot.Tele.Run as Tele
import qualified Bot.Vk.Run as Vk
import qualified Bot.LoggerIO as BLIO
import qualified Bot.Logger as BL

import Bot.Settings (config)

main :: IO ()
main = do
  -- handle log
  logh <- BLIO.newHandle
  api <- fmap Config.botApi config
  case api of
    "telegram" -> Tele.run logh
    "vk" -> Vk.run logh
    _ -> BL.logError logh "Incorrect bot_api in config.json"

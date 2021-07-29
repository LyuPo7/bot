{-# LANGUAGE OverloadedStrings #-}

module Tele.Parser where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as B
import Data.Aeson (eitherDecode)

import Config
import Tele.Types
import Logger
import LoggerIO

{-- | UpdateData parser --}
parseUpdateData :: Logger.Handle -> IO L8.ByteString -> IO UpdateData
parseUpdateData logh response = do
  -- decode JSON 
  d <- (eitherDecode <$> response) :: IO (Either String UpdateData)
  case d of
    Left err -> do
      logError logh $ "Couldn't parse UpdateData: " ++ err
      return UpdateData {ok = True, result = []}
    Right ps -> do
      logDebug logh "UpdateData was successfully parsed."
      return ps

{-- | PollData parser --}
parsePollData :: Logger.Handle -> IO L8.ByteString -> IO PollData
parsePollData logh response = do
 -- decode JSON 
 d <- (eitherDecode <$> response) :: IO (Either String PollData)
 case d of
  Left err -> do
    logError logh $ "Couldn't parse poll response: " ++ err
    fail err --main = toTry `catchIOError` handler
  Right ps -> do
    logDebug logh "Poll response was successfully parsed."
    return ps

{-- | Config parser --}
parseBotConfig :: IO B.ByteString -> IO BotConfig
parseBotConfig config = do
  -- Get JSON data and decode it
  d <- (eitherDecode <$> config) :: IO (Either String BotConfig)
  case d of
    Left err -> do
      fail err --main = toTry `catchIOError` handler
    Right ps -> do
      --logDebug logh "Poll response was successfully parsed."
      return ps
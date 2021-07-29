{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Data.Text (Text, pack)
import qualified Data.ByteString.Lazy as B

import Config
import Tele.Types
import Tele.Parser (parseBotConfig)
import qualified Data.ByteString.Char8 as BC

-- Host
newtype Host = Host { getHost :: BC.ByteString }

-- bot commands
startMessage = pack "/start"
helpMessage = pack "/help"
repeatMessage = pack "/repeat"

-- bot modes
reply = pack "reply"
answer = pack "answer"

-- api host
apiTele = Host "https://api.telegram.org/bot"
apiVk = Host "https://api.vk.com/method/"

-- api version
vkVersion = pack "5.80"

-- Config
configFile :: FilePath
configFile = "src/files/config.json"

-- Read the config JSON file.
getConfig :: IO B.ByteString
getConfig = B.readFile configFile

-- Get settings from config
config = parseBotConfig getConfig
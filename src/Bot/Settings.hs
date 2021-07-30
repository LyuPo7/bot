{-# LANGUAGE OverloadedStrings #-}

module Bot.Settings where

import Data.Text (Text, pack)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC

import Bot.Config
import Bot.Tele.Request.Data()

-- | Types for Settings
-- | Host
newtype Host = Host { getHost :: BC.ByteString }

-- | Bot commands
startMessage, helpMessage, repeatMessage :: Text
startMessage = pack "/start" -- initialize chat with new User;
helpMessage = pack "/help"   -- request bot description;
repeatMessage = pack "/repeat" -- request for change reply number;

-- | Bot modes
reply, answer :: Text
reply = pack "reply" -- in this mode: Bot replies for every User's message;
answer = pack "answer" -- in this mode: Bot tries to recieve new reply number from User;

-- | Api host
apiTele, apiVk :: Host
apiTele = Host "https://api.telegram.org/bot"
apiVk = Host "https://api.vk.com/method/"

-- | Api version
vkVersion :: Text
vkVersion = pack "5.80"

-- | Config file
configFile :: FilePath
configFile = "src/Bot/files/config_tele.json"

-- | Read the config JSON file.
getConfig :: IO B.ByteString
getConfig = B.readFile configFile

-- | Get settings from config
config :: IO Config
config = parseConfig getConfig
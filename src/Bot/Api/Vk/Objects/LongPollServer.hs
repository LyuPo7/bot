{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Api.Vk.Objects.LongPollServer where

import Web.FormUrlEncoded (ToForm(..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Bot.Objects.Synonyms as BotSynonyms

data LongPollServer = LongPollServer {
  key :: Text,
  ts :: Integer,
  act :: Text,
  wait :: BotSynonyms.Timeout
} deriving (Generic, ToForm)
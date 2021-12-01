{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.LongPollServer where

import Data.Text (Text)
import GHC.Generics (Generic)
import Web.FormUrlEncoded (ToForm (..))

import qualified Bot.Objects.Synonyms as BotSynonyms

data LongPollServer = LongPollServer
  { key :: Text,
    ts :: Integer,
    act :: Text,
    wait :: BotSynonyms.Timeout
  }
  deriving (Generic, ToForm)

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot.Vk.Request.Objects.LongPollServer where

import Web.FormUrlEncoded (ToForm(..))
import Data.Text (Text)
import GHC.Generics (Generic)

data LongPollServer = LongPollServer {
  key :: Text,
  ts :: Integer,
  act :: Text,
  wait :: Integer
} deriving (Generic, ToForm)
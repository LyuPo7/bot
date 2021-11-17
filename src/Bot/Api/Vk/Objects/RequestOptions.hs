module Bot.Api.Vk.Objects.RequestOptions where

import Data.Text (Text)

newtype RequestOptions = RequestOptions {
  reqOption :: Text
} deriving (Show, Eq)
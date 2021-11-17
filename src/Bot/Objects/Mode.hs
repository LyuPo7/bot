{-# LANGUAGE MultiParamTypeClasses #-}
module Bot.Objects.Mode where

import Database.HDBC (SqlValue(..))
import Data.Convertible.Base (Convertible(..))

data Mode = ReplyMode
          | AnswerMode
          | UnknownMode
          deriving (Show, Eq)

instance Convertible Mode SqlValue where
  safeConvert ReplyMode = Right $ SqlByteString "reply"
  safeConvert AnswerMode = Right $ SqlByteString "answer"
  safeConvert UnknownMode = Right $ SqlByteString ""

instance Convertible SqlValue Mode where
  safeConvert (SqlByteString "reply") = Right ReplyMode
  safeConvert (SqlByteString "answer") = Right AnswerMode
  safeConvert _ = Right UnknownMode
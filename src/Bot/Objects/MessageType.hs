module Bot.Objects.MessageType where

import Data.Convertible.Base (Convertible (..))
import Data.Text (Text)
import qualified Data.Text as T

data MessageType
  = TextMessage Text
  | HelpMessage
  | RepeatMessage
  | StartMessage
  | UnsupportedMessage
  deriving (Show, Eq)

instance Convertible Text MessageType where
  safeConvert "/help" = Right HelpMessage
  safeConvert "/start" = Right StartMessage
  safeConvert "/repeat" = Right RepeatMessage
  safeConvert text =
    if T.isPrefixOf "/" text
      then Right UnsupportedMessage
      else Right $ TextMessage text

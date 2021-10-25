module Bot.Util where

import qualified Data.Text as T
import Data.Text (Text)

-- | Convert value to Text
convert :: Show a => a -> Text
convert = T.pack . show
module Bot.Util where

import qualified Data.Text as T
import Data.Text (Text)
import Text.Read (readEither)

convertValue :: Show a => a -> Text
convertValue = T.pack . show

readValue :: (Monad m, Read a) => Text -> m (Either Text a)
readValue arg = case readEither $ T.unpack arg of
  Right y -> return $ Right y
  Left _ -> return $ Left $ "Incorrect value: " <> arg
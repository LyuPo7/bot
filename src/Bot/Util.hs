{-# LANGUAGE OverloadedStrings #-}

module Bot.Util where

import qualified Data.Text as T
import Data.Text (Text)
import Text.Read (readEither)

-- | Convert value to Text
convert :: Show a => a -> Text
convert = T.pack . show

-- | Read Key
readEitherMa :: (Monad m, Read a) => Text -> m (Either Text a)
readEitherMa arg = case readEither $ T.unpack arg of
  Right y -> return $ Right y
  Left _ -> return $ Left $ "Incorrect key: " <> arg
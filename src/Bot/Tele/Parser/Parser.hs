{-# LANGUAGE OverloadedStrings #-}

module Bot.Tele.Parser.Parser where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson (eitherDecode)

import qualified Bot.Logger as BL
import Bot.Tele.Parser.Data (UpdateData(..))

{-- | UpdateData parser --}
parseUpdateData :: BL.Handle -> IO L8.ByteString -> IO UpdateData
parseUpdateData logh response = do
  -- decode JSON 
  d <- (eitherDecode <$> response) :: IO (Either String UpdateData)
  case d of
    Left err -> do
      BL.logError logh $ "Couldn't parse UpdateData: " ++ err
      return UpdateData {ok = True, result = []}
    Right ps -> do
      BL.logDebug logh "UpdateData was successfully parsed."
      return ps
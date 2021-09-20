{-# LANGUAGE OverloadedStrings #-}

module Bot.Tele.Parser.Parser where

import qualified Data.Text as T
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Aeson (eitherDecode)

import Bot.Tele.Parser.ParserSpec (Handle(..))
import qualified Bot.Settings as Settings
import qualified Bot.Logger as Logger
import Bot.Tele.Parser.Data

withHandleIO :: Logger.Handle IO -> Settings.Config -> (Handle IO -> IO a) -> IO a
withHandleIO logger cParser f = do
  let handle = Handle logger cParser
  f handle

{-- | UpdateData parser --}
parseUpdateData :: Monad m => Handle m -> ByteString -> m UpdateData
parseUpdateData handle response = do
  let logh = hLogger handle
      -- decode JSON 
      d = eitherDecode response :: (Either String UpdateData)
  case d of
    Left err -> do
      Logger.logError logh ("Couldn't parse UpdateData: " <> T.pack err)
      return UpdateData {ok = True, result = []}
    Right ps -> do
      Logger.logDebug logh "UpdateData was successfully parsed."
      return ps
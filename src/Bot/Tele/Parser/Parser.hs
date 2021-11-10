module Bot.Tele.Parser.Parser where

import qualified Data.Text as T
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Aeson (eitherDecode)

import Bot.Tele.Parser.ParserSpec (Handle(..))
import qualified Bot.Settings as Settings
import qualified Bot.Logger as Logger
import Bot.Tele.Parser.Objects.UpdateData (UpdateData(..))

withHandleIO :: Logger.Handle IO ->
                Settings.Config -> (Handle IO -> IO a) -> IO a
withHandleIO logger cParser f = do
  let handle = Handle logger cParser
  f handle

{-- | UpdateData parser --}
parseUpdateData :: Monad m => Handle m -> ByteString -> m UpdateData
parseUpdateData handle response = do
  let logH = hLogger handle
      -- decode JSON 
      d = eitherDecode response :: (Either String UpdateData)
  case d of
    Left err -> do
      Logger.logError logH $"Couldn't parse UpdateData: "
        <> T.pack err
      return UpdateData {ok = True, result = []}
    Right ps -> do
      Logger.logDebug logH "UpdateData was successfully parsed."
      return ps
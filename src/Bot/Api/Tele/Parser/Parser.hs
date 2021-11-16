module Bot.Api.Tele.Parser.Parser where

import qualified Data.Text as T
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Aeson (eitherDecode)

import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Settings as Settings
import qualified Bot.Logger.Logger as Logger
import qualified Bot.System.System as BotSystem
import qualified Bot.Api.Tele.Objects.UpdateData as TeleUpData

withHandleIO :: Logger.Handle IO -> BotSystem.Handle IO ->
                Settings.Config -> (BotParser.Handle IO -> IO a) -> IO a
withHandleIO logger sysH cParser f = do
  let handle = BotParser.Handle logger sysH cParser
  f handle

parseUpdateData :: Monad m => BotParser.Handle m -> ByteString -> m TeleUpData.UpdateData
parseUpdateData handle response = do
  let logH = BotParser.hLogger handle
      d = eitherDecode response :: (Either String TeleUpData.UpdateData)
  case d of
    Left err -> do
      Logger.logError logH $"Couldn't parse UpdateData: "
        <> T.pack err
      return TeleUpData.UpdateData {
        TeleUpData.ok = True,
        TeleUpData.result = []}
    Right ps -> do
      Logger.logDebug logH "UpdateData was successfully parsed."
      return ps
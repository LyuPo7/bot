module Bot.Api.Tele.Parser.Parser where

import qualified Data.Text as T
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Aeson (eitherDecode)

import qualified Bot.Parser.Parser as BotParser
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Logger.Logger as Logger
import qualified Bot.Api.Tele.Objects.UpdateData as TeleUpData

parseUpdateData :: Monad m =>
                   BotParser.Handle m ->
                   ByteString ->
                   m TeleUpData.UpdateData
parseUpdateData handle response = do
  let logH = BotParser.hLogger handle
      d = eitherDecode response :: (Either String TeleUpData.UpdateData)
  case d of
    Left err -> do
      Logger.logError logH $"Couldn't parse UpdateData: "
        <> T.pack err
      return TeleUpData.UpdateData {
        TeleUpData.ok = BotSynonyms.Status True,
        TeleUpData.result = []}
    Right ps -> do
      Logger.logDebug logH "UpdateData was successfully parsed."
      return ps
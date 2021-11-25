module Bot.System.System where

import qualified Data.ByteString.Lazy as B

import qualified Bot.Logger.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Objects.Synonyms as BotSynonyms

data Handle m = Handle {
  hLogger :: Logger.Handle m,
  cSet :: Settings.Config,

  readFile :: FilePath ->
              m String,
  getTemporaryDirectory :: m FilePath,
  downloadFile :: BotSynonyms.Url ->
                  FilePath -> m (),
  uploadFile :: BotSynonyms.Url ->
                FilePath ->
                m B.ByteString
}
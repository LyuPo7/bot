module Bot.System.System where

import qualified Data.ByteString.Lazy as B
import Data.Text (Text)

import qualified Bot.Logger.Logger as Logger
import qualified Bot.Settings as Settings

data Handle m = Handle {
  hLogger :: Logger.Handle m,
  cSet :: Settings.Config,

  readFile :: FilePath -> m String,
  getTemporaryDirectory :: m FilePath,
  downloadFile :: Text -> FilePath -> m (),
  uploadFile :: Text -> FilePath -> m B.ByteString
}
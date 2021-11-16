module Bot.DB.DB where

import Database.HDBC.Sqlite3 (Connection)

import qualified Bot.Logger.Logger as Logger
import qualified Bot.Settings as Settings

data Handle m = Handle {
  hLogger :: Logger.Handle m,
  conn :: Connection,
  cDb :: Settings.Config
}
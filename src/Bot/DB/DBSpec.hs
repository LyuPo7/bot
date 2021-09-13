module Bot.DB.DBSpec where

import Database.HDBC.Sqlite3 (Connection)

import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings

data Handle m = Handle {
    hLogger :: Logger.Handle m,
    hDb :: Connection,
    configDb :: Settings.Config
}
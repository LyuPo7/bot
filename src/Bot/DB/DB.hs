module Bot.DB.DB where

import Database.HDBC.Sqlite3 (Connection)

import qualified Bot.Logger.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.System.System as BotSystem

data Handle m = Handle
  { hLogger :: Logger.Handle m,
    conn :: Connection,
    hSystem :: BotSystem.Handle m,
    cDb :: Settings.Config
  }

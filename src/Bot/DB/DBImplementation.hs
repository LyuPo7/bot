{-# LANGUAGE FlexibleContexts #-}

module Bot.DB.DBImplementation where

import Database.HDBC (getTables, run, commit)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Control.Monad (when)
import qualified Control.Exception as Exc

import qualified Bot.DB.DB as BotDB
import qualified Bot.Logger.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Objects.Api as BotApi
import qualified Bot.Exception as E

withHandleIO :: Logger.Handle IO ->
                Settings.Config -> (BotDB.Handle IO -> IO a) -> IO a
withHandleIO logger config f = do
  case Settings.botApi config of
    BotApi.Vk -> do
      let dbFile = "data/vk.db"
      dbConn <- connect dbFile
      let handle = BotDB.Handle logger dbConn config
      prepDB handle
      f handle
    BotApi.Tele -> do
      let dbFile = "data/tele.db"
      dbConn <- connect dbFile
      let handle = BotDB.Handle logger dbConn config
      prepDB handle
      f handle
    _ -> do
      Logger.logError logger "Incorrect field 'bot_api' \
                             \in config.json"
      Exc.throwIO $ E.ParseConfigError "Incorrect field 'bot_api' \
                                       \in config.json"

connect :: FilePath -> IO Connection
connect dbFile = do connectSqlite3 dbFile

{- | Prepare the database for data.
Create two tables and ask the database engine to verify some info:
** Table: rep_numbers (table contains info about reply_number for every Chat):
    - chat_id - unique identifier for this message;
    - reply_number - number of replies for response;
** Table: modes (table contains mode in which bot work for every Chat):
    - chat_id - unique identifier for this message;
    - mode - work mode;
** Table: updates (table contains info about update's status):
    - update_id - unique identifier for this update;
    - processed - status the response;
-}
prepDB :: BotDB.Handle IO -> IO ()
prepDB handle = do
  let dbConn = BotDB.conn handle
      logH = BotDB.hLogger handle
  tables <- getTables dbConn
  when ("rep_numbers" `notElem` tables) $ do
    _ <- run dbConn "CREATE TABLE rep_numbers (\
                    \chat_id INTEGER NOT NULL PRIMARY KEY UNIQUE,\
                    \reply_number INTEGER NOT NULL)" []
    Logger.logInfo logH "Table 'rep_numbers' was successfully created!"
  when ("modes" `notElem` tables) $ do 
    _ <- run dbConn "CREATE TABLE modes (\
                    \chat_id INTEGER NOT NULL PRIMARY KEY UNIQUE,\
                    \mode TEXT NOT NULL)" []
    Logger.logInfo logH "Table 'modes' was successfully created!"
  when ("updates" `notElem` tables) $ do 
    _ <- run dbConn "CREATE TABLE updates (\
                    \update_ID INTEGER NOT NULL PRIMARY KEY UNIQUE,\
                    \processed BOOLEAN)" []
    Logger.logInfo logH "Table 'updates' was successfully created!"
  commit dbConn
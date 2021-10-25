{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Bot.DB.DBQueries where

import Database.HDBC (handleSql, getTables, run, commit, quickQuery', fromSql, toSql)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Control.Monad (when)
import Data.Text (Text, pack)
import qualified Control.Exception as Exc

import Bot.DB.DBSpec (Handle(..))
import qualified Bot.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Exception as E
import Bot.Tele.Parser.Data
import Bot.Util (convert)

withHandleIO :: Logger.Handle IO -> Settings.Config -> (Handle IO -> IO a) -> IO a
withHandleIO logger config f = do
    case Settings.botApi config of
        "vk" -> do
            let dbFile = "data/vk.db"
            dbh <- connect dbFile
            let handle = Handle logger dbh config
            prepDB handle
            f handle
        "telegram" -> do
            let dbFile = "data/tele.db"
            dbh <- connect dbFile
            let handle = Handle logger dbh config
            prepDB handle
            f handle
        _ -> do
            Logger.logError logger "Incorrect field 'bot_api' in config.json"
            Exc.throwIO $ E.ParseConfigError "Incorrect field 'bot_api' in config.json"

-- | Initialize DB and return database Connection
connect :: FilePath -> IO Connection
connect dbFile = do connectSqlite3 dbFile

{- | Prepare the database for data.
Create two tables and ask the database engine to verify some info:
** Table: rep_numbers (table contains info about reply_number for every Chat):
    - chat_id - unique identifier for this message;
    - reply_number - number of replies for responce;
** Table: modes (table contains mode in which bot work for every Chat):
    - chat_id - unique identifier for this message;
    - mode - work mode;
** Table: updates (table contains info about update's status):
    - update_id - unique identifier for this update;
    - processed - status the responce;
-}
prepDB :: Handle IO -> IO ()
prepDB handle = do
    let dbh = hDb handle
        logh = hLogger handle
    tables <- getTables dbh
    when ("rep_numbers" `notElem` tables) $ do
            _ <- run dbh "CREATE TABLE rep_numbers (\
                       \chat_id INTEGER NOT NULL PRIMARY KEY UNIQUE,\
                       \reply_number INTEGER NOT NULL)" []
            Logger.logInfo logh "Table 'rep_numbers' was successfully created!"
            return ()
    when ("modes" `notElem` tables) $ do 
            _ <- run dbh "CREATE TABLE modes (\
                       \chat_id INTEGER NOT NULL PRIMARY KEY UNIQUE,\
                       \mode TEXT NOT NULL)" []
            Logger.logInfo logh "Table 'modes' was successfully created!"
            return ()
    when ("updates" `notElem` tables) $ do 
            _ <- run dbh "CREATE TABLE updates (\
                       \update_ID INTEGER NOT NULL PRIMARY KEY UNIQUE,\
                       \processed BOOLEAN)" []
            Logger.logInfo logh "Info: Table 'updates' was successfully created!"
            return ()
    commit dbh

{- | Gets id of last successfully processed update from DB -}
getLastSucUpdate :: Handle IO -> IO (Maybe UpdateID)
getLastSucUpdate handle = handleSql errorHandler $ do
    let dbh = hDb handle
        logh = hLogger handle
    r <- quickQuery' dbh "SELECT update_ID FROM updates ORDER BY update_ID DESC LIMIT 1" []
    case r of
        [[x]] -> do
            Logger.logInfo logh $ "Last processed update with id: " <> convert (fromSql x :: Integer)
            return (Just $ fromSql x)
        _ -> do
            Logger.logWarning logh "There are no processed updates for now"
            return Nothing
    where errorHandler _ = 
              do Exc.throwIO $ E.DbError "Error: Error in getLastSucUpdate!"

putUpdate :: Handle IO -> UpdateID -> IO ()
putUpdate handle updateID = handleSql errorHandler $ do
    let dbh = hDb handle
        logh = hLogger handle
    r <- quickQuery' dbh "SELECT update_ID FROM updates WHERE update_ID = ?" 
                  [toSql updateID]
    case r of
        [] -> do
            _ <- run dbh "INSERT INTO updates (update_ID, processed) VALUES (?,?)"
                        [toSql updateID, toSql True]
            commit dbh
            Logger.logInfo logh $ "Update with id: " <> convert updateID <> " was successfully inserted in db."
        _ -> Logger.logWarning logh $ "Update with id: " <> convert updateID <> " already exists in db."
    where errorHandler e = 
              do Exc.throwIO $ E.DbError $ "Error: Error in putUpdate!\n"
                     ++ show e

{- | Gets a reply number for given Chat from the database (table rep_numbers).
     If no exists data for given Chat in db then return initial number from config. -}
getRepliesNumber :: Handle IO -> ChatID -> IO RepNum
getRepliesNumber handle chatId = handleSql errorHandler $ do
    let dbh = hDb handle
        logh = hLogger handle
        config = configDb handle
    -- check if chat_id already in the table:
    r <- quickQuery' dbh "SELECT reply_number FROM rep_numbers WHERE chat_ID = ?"
              [toSql chatId]
    case r of
        [[x]] -> do
            Logger.logInfo logh $ "Info: Will use: " <> convert (fromSql x :: Integer) <> " replies for Chat with id: "  <> convert chatId
            return $ fromSql x
        _ -> do
            Logger.logInfo logh $ "Will use initial number of replies for Chat with id: " <> convert chatId
            return $ Settings.botInitialReplyNumber config -- return initial reply number
    where errorHandler e = 
              do Exc.throwIO $ E.DbError $ "Error: Error in getRepliesNumber!\n"
                     ++ show e

{- | Sets a reply number for given Chat to the database (table rep_numbers).
     If no exists data for given Chat in db then sets the given number
     If chatId exists reply_number in db then change existed reply_number. -}
setRepliesNumber :: Handle IO -> ChatID -> RepNum -> IO ()
setRepliesNumber handle chatId repNum = handleSql errorHandler $ do
    let dbh = hDb handle
        logh = hLogger handle
    -- check if chat_id already in the table:
    r <- quickQuery' dbh "SELECT reply_number FROM rep_numbers WHERE chat_ID = ?"
              [toSql chatId]
    case r of
        [] -> do
            _ <- run dbh "INSERT INTO rep_numbers (chat_id, reply_number) VALUES (?,?)"
                    [toSql chatId, toSql repNum]
            commit dbh
            Logger.logInfo logh $ "Reply_number for Chat with id: " <> convert chatId <> " was successfully inserted in db."
        _ -> do
            _ <- run dbh "UPDATE rep_numbers SET reply_number = ? WHERE chat_id = ?"
                    [toSql repNum, toSql chatId]
            commit dbh
            Logger.logInfo logh $ "Info: Reply_number for Chat with id: " <> convert chatId <> " was successfully changed in db."
    where errorHandler e = 
              do Exc.throwIO $ E.DbError $ "Error: Error in setRepliesNumber!\n"
                     <> show e

{- | Gets a mode for given Chat from the database (table modes).
     If no exists data for given Chat in db then return default mode. -}
getMode :: Handle IO -> ChatID -> IO Text
getMode handle chatId = handleSql errorHandler $ do
    let dbh = hDb handle
        logh = hLogger handle
    -- check if chat_id already in the table:
    r <- quickQuery' dbh "SELECT mode FROM modes WHERE chat_id = ?"
              [toSql chatId]
    case r of
        [[x]] -> do
            Logger.logInfo logh $ "Will use: " <> pack (fromSql x :: String) <> " mode for Chat with id: "  <> convert chatId
            return $ fromSql x
        _ -> do
            Logger.logInfo logh $ "Will use default mode for Chat with id: " <> convert chatId
            return "reply" -- return initial reply number
    where errorHandler e = 
              do Exc.throwIO $ E.DbError $ "Error in getMode!\n"
                     ++ show e

{- | Sets a mode for given Chat to the database (table modes).
     If no exists data for given Chat in db then sets the given number
     If chatId exists mode in db then change existed mode. -}
setMode :: Handle IO -> ChatID -> Mode -> IO ()
setMode handle chatId mode = handleSql errorHandler $ do
    let dbh = hDb handle
        logh = hLogger handle
    -- check if chat_id already in the table:
    r <- quickQuery' dbh "SELECT mode FROM modes WHERE chat_id = ?"
              [toSql chatId]
    case r of
        [] -> do
            _ <- run dbh "INSERT INTO modes (chat_id, mode) VALUES (?,?)"
                    [toSql chatId, toSql mode]
            commit dbh
            Logger.logInfo logh $ "Mode for Chat with id: " <> convert chatId <> " was successfully inserted in db."
        _ -> do
            _ <- run dbh "UPDATE modes SET mode = ? WHERE chat_ID = ?"
                    [toSql mode, toSql chatId]
            commit dbh
            Logger.logInfo logh $ "Mode for Chat with id: " <> convert chatId <> " was successfully changed in db."
    where errorHandler e = 
              do Exc.throwIO $ E.DbError $ "Error in setMode!\n"
                     ++ show e
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module DB where

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad (when, liftM)
import Data.List(sort)
import Data.Text (Text)
import Control.Monad.Except (throwError)

import Settings (config)
import Config
import Logger
import LoggerIO
import Exception
import Tele.Types

-- | Initialize DB and return database Connection
connect :: Logger.Handle -> FilePath -> IO Connection
connect logh dbFile =
    do dbHandle <- connectSqlite3 dbFile
       prepDB logh dbHandle
       return dbHandle

{- | Prepare the database for data.
Create two tables and ask the database engine to verify some info:
** Table: rep_numbers (table contains info about reply_number for every user):
    - user_id - unique identifier for this message;
    - reply_number - number of replies for responce;
** Table: modes (table contains mode in which bot work for every user):
    - user_id - unique identifier for this message;
    - mode - work mode;
** Table: updates (table contains info about update's status):
    - update_id - unique identifier for this update;
    - processed - status the responce;
-}
prepDB :: IConnection conn => Logger.Handle -> conn -> IO ()
prepDB logh dbHandle =
    do tables <- getTables dbHandle
       when ("rep_numbers" `notElem` tables) $
           do run dbHandle "CREATE TABLE rep_numbers (\
                       \user_ID INTEGER NOT NULL PRIMARY KEY UNIQUE,\
                       \reply_number INTEGER NOT NULL)" []
              logInfo logh "Table 'rep_numbers' was successfully created!"
              return ()
       when ("modes" `notElem` tables) $
           do run dbHandle "CREATE TABLE modes (\
                       \user_ID INTEGER NOT NULL PRIMARY KEY UNIQUE,\
                       \mode TEXT NOT NULL)" []
              logInfo logh "Table 'modes' was successfully created!"
              return ()
       when ("updates" `notElem` tables) $
           do run dbHandle "CREATE TABLE updates (\
                       \update_ID INTEGER NOT NULL PRIMARY KEY UNIQUE,\
                       \processed BOOLEAN)" []
              logInfo logh "Info: Table 'updates' was successfully created!"
              return ()
       commit dbHandle

{- | Gets id if last successfully processed update from DB -}
getLastSucUpdate :: IConnection conn => Logger.Handle -> conn -> IO (Maybe Integer)
getLastSucUpdate logh dbh =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT update_ID FROM updates ORDER BY update_ID DESC LIMIT 1" []
        case r of
            [] -> do
                logWarning logh "There are no processed updates for now"
                return Nothing --return (Right (-1))
            [[x]] -> do
                logInfo logh $ "Last processed update with id: " ++ show (fromSql x :: Integer)
                return (Just $ fromSql x) --return (Right $ fromSql x)
    where errorHandler err = 
              do fail "Error: Error in getLastSucUpdate!\n" -- return $ throwError $ DbError $ show err

putUpdate :: IConnection conn => Logger.Handle -> conn -> Integer -> IO ()
putUpdate logh dbh updateID =
    handleSql errorHandler $ do
        r <- quickQuery' dbh "SELECT update_ID FROM updates WHERE update_ID = ?" 
                  [toSql updateID]
        case r of
            [[x]] -> logWarning logh $ "Update with id: " ++ show updateID ++ " already exists in db."
            [] -> do
                run dbh "INSERT INTO updates (update_ID, processed) VALUES (?,?)"
                        [toSql updateID, toSql True]
                commit dbh
                logInfo logh $ "Update with id: " ++ show updateID ++ " was successfully inserted in db."
    where errorHandler e = 
              do fail $ "Error: Error in putUpdate!\n"
                     ++ show e

{- | Gets a reply number for given User from the database (table settings).
     If no data for given User in db then return initial number from config. -}
getRepliesNumber :: IConnection conn => Logger.Handle -> conn -> Integer -> IO Integer
getRepliesNumber logh dbh userID =
    handleSql errorHandler $ do
      -- check if user_id already in the table:
      r <- quickQuery' dbh "SELECT reply_number FROM rep_numbers WHERE user_ID = ?"
              [toSql userID]
      case r of
          [] -> do
              logInfo logh $ "Will use initial number of replies for User with id: " ++ show userID
              fmap botInitialReplyNumber config -- return initial reply number
          [[x]] -> do
              logInfo logh $ "Info: Will use: " ++ show (fromSql x :: Integer) ++ " replies for User with id: "  ++ show userID
              return $ fromSql x
    where errorHandler e = 
              do fail $ "Error: Error in getRepliesNumber!\n"
                     ++ show e

{- | Sets a reply number for given User to the database (table settings).
     If no data for given User in db then sets the given number
     If userID exists reply_number in db then change existed reply_number. -}
setRepliesNumber :: IConnection conn => Logger.Handle -> conn -> Integer -> Integer -> IO ()
setRepliesNumber logh dbh userID repNum =
    handleSql errorHandler $ do
      -- check if user_id already in the table:
      r <- quickQuery' dbh "SELECT reply_number FROM rep_numbers WHERE user_ID = ?"
              [toSql userID]
      case r of
          [] -> do
            run dbh "INSERT INTO rep_numbers (user_ID, reply_number) VALUES (?,?)"
                    [toSql userID, toSql repNum]
            commit dbh
            logInfo logh $ "Reply_number for User with id: " ++ show userID ++ " was successfully inserted in db."
          [[x]] -> do
            run dbh "UPDATE rep_numbers SET reply_number = ? WHERE user_ID = ?"
                    [toSql repNum, toSql userID]
            commit dbh
            logInfo logh $ "Info: Reply_number for User with id: " ++ show userID ++ " was successfully changed in db."
    where errorHandler e = 
              do fail $ "Error: Error in setRepliesNumber!\n"
                     ++ show e

{- | Gets a mode for given User from the database (table rep_numbers).
     If no data for given User in db then return default mode. -}
getMode :: IConnection conn => Logger.Handle -> conn -> Integer -> IO Text
getMode logh dbh userID =
    handleSql errorHandler $ do
      -- check if user_id already in the table:
      r <- quickQuery' dbh "SELECT mode FROM modes WHERE user_ID = ?"
              [toSql userID]
      case r of
          [] -> do
              logInfo logh $ "Will use default mode for User with id: " ++ show userID
              return "reply" -- return initial reply number
          [[x]] -> do
              logInfo logh $ "Will use: " ++ show (fromSql x :: String) ++ " mode for User with id: "  ++ show userID
              return $ fromSql x
    where errorHandler e = 
              do fail $ "Error in getMode!\n"
                     ++ show e

{- | Sets a mode for given User to the database (table modes).
     If no data for given User in db then sets the given number
     If userID exists mode in db then change existed mode. -}
setMode :: IConnection conn => Logger.Handle -> conn -> Integer -> Text -> IO ()
setMode logh dbh userID mode =
    handleSql errorHandler $ do
      -- check if user_id already in the table:
      r <- quickQuery' dbh "SELECT mode FROM modes WHERE user_ID = ?"
              [toSql userID]
      case r of
          [] -> do
            run dbh "INSERT INTO modes (user_ID, mode) VALUES (?,?)"
                    [toSql userID, toSql mode]
            commit dbh
            logInfo logh $ "Mode for User with id: " ++ show userID ++ " was successfully inserted in db."
          [[x]] -> do
            run dbh "UPDATE modes SET mode = ? WHERE user_ID = ?"
                    [toSql mode, toSql userID]
            commit dbh
            logInfo logh $ "Mode for User with id: " ++ show userID ++ " was successfully changed in db."
    where errorHandler e = 
              do fail $ "Error in setMode!\n"
                     ++ show e
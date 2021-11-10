{-# LANGUAGE FlexibleContexts #-}

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
import Bot.Tele.Parser.Objects.Synonyms (UpdateId, ChatId, Mode, RepNum)
import Bot.Util (convert)

withHandleIO :: Logger.Handle IO ->
                Settings.Config -> (Handle IO -> IO a) -> IO a
withHandleIO logger config f = do
  case Settings.botApi config of
    "vk" -> do
      let dbFile = "data/vk.db"
      dbH <- connect dbFile
      let handle = Handle logger dbH config
      prepDB handle
      f handle
    "telegram" -> do
      let dbFile = "data/tele.db"
      dbH <- connect dbFile
      let handle = Handle logger dbH config
      prepDB handle
      f handle
    _ -> do
      Logger.logError logger "Incorrect field 'bot_api' \
                             \in config.json"
      Exc.throwIO $ E.ParseConfigError "Incorrect field 'bot_api' \
                                       \in config.json"

-- | Initialize DB and return database Connection
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
prepDB :: Handle IO -> IO ()
prepDB handle = do
  let dbH = hDb handle
      logH = hLogger handle
  tables <- getTables dbH
  when ("rep_numbers" `notElem` tables) $ do
    _ <- run dbH "CREATE TABLE rep_numbers (\
                 \chat_id INTEGER NOT NULL PRIMARY KEY UNIQUE,\
                 \reply_number INTEGER NOT NULL)" []
    Logger.logInfo logH "Table 'rep_numbers' was successfully created!"
  when ("modes" `notElem` tables) $ do 
    _ <- run dbH "CREATE TABLE modes (\
                 \chat_id INTEGER NOT NULL PRIMARY KEY UNIQUE,\
                 \mode TEXT NOT NULL)" []
    Logger.logInfo logH "Table 'modes' was successfully created!"
  when ("updates" `notElem` tables) $ do 
    _ <- run dbH "CREATE TABLE updates (\
                 \update_ID INTEGER NOT NULL PRIMARY KEY UNIQUE,\
                 \processed BOOLEAN)" []
    Logger.logInfo logH "Table 'updates' was successfully created!"
  commit dbH

{- | Gets id of last successfully processed update from DB -}
getLastSucUpdate :: Handle IO -> IO (Maybe UpdateId)
getLastSucUpdate handle = handleSql errorHandler $ do
  let dbH = hDb handle
      logH = hLogger handle
  r <- quickQuery' dbH "SELECT update_ID \
                       \FROM updates \
                       \ORDER BY update_ID DESC \
                       \LIMIT 1" []
  case r of
    [[x]] -> do
      Logger.logInfo logH $ "Last processed update with id: "
        <> convert (fromSql x :: UpdateId)
      return $ Just $ fromSql x
    _ -> do
      Logger.logWarning logH "There are no processed updates for now"
      return Nothing
  where errorHandler _ = do
          Exc.throwIO $ E.DbError "Error: Error in getLastSucUpdate!"

putUpdate :: Handle IO -> UpdateId -> IO ()
putUpdate handle updateId = handleSql errorHandler $ do
  let dbH = hDb handle
      logH = hLogger handle
  r <- quickQuery' dbH "SELECT update_ID \
                       \FROM updates \
                       \WHERE update_ID = ?" 
        [toSql updateId]
  case r of
    [] -> do
      _ <- run dbH "INSERT INTO updates (update_ID, processed) \
                   \VALUES (?,?)"
            [toSql updateId, toSql True]
      commit dbH
      Logger.logInfo logH $ "Update with id: " 
        <> convert updateId 
        <> " was successfully inserted in db."
    _ -> Logger.logWarning logH $ "Update with id: "
           <> convert updateId
           <> " already exists in db."
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in putUpdate!\n"
            <> show e

{- | Gets a reply number for given Chat from the database (table rep_numbers).
     If no exists data for given Chat in db then return initial number from config. -}
getRepliesNumber :: Handle IO -> ChatId -> IO RepNum
getRepliesNumber handle chatId = handleSql errorHandler $ do
  let dbH = hDb handle
      logH = hLogger handle
      config = configDb handle
  -- check if chat_id already in the table:
  r <- quickQuery' dbH "SELECT reply_number \
                       \FROM rep_numbers \
                       \WHERE chat_ID = ?"
        [toSql chatId]
  case r of
    [[x]] -> do
      Logger.logInfo logH $ "Will use: " 
        <> convert (fromSql x :: RepNum)
        <> " replies for Chat with id: " 
        <> convert chatId
      return $ fromSql x
    _ -> do
      Logger.logInfo logH 
        $ "Will use initial number of replies for Chat with id: " 
          <> convert chatId
      return $ Settings.botInitialReplyNumber config
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getRepliesNumber!\n"
            <> show e

{- | Sets a reply number for given Chat to the database (table rep_numbers).
     If no exists data for given Chat in db then sets the given number
     If chatId exists reply_number in db then change existed reply_number. -}
setRepliesNumber :: Handle IO -> ChatId -> RepNum -> IO ()
setRepliesNumber handle chatId repNum = handleSql errorHandler $ do
  let dbH = hDb handle
      logH = hLogger handle
  -- check if chat_id already in the table:
  r <- quickQuery' dbH "SELECT reply_number \
                       \FROM rep_numbers \
                       \WHERE chat_ID = ?"
        [toSql chatId]
  case r of
    [] -> do
      _ <- run dbH "INSERT INTO rep_numbers (chat_id, reply_number) \
                   \VALUES (?,?)"
            [toSql chatId, toSql repNum]
      commit dbH
      Logger.logInfo logH $ "Reply_number for Chat with id: "
        <> convert chatId 
        <> " was successfully inserted in db."
    _ -> do
      _ <- run dbH "UPDATE rep_numbers \
                   \SET reply_number = ? \
                   \WHERE chat_id = ?"
            [toSql repNum, toSql chatId]
      commit dbH
      Logger.logInfo logH $ "Info: Reply_number for Chat with id: "
        <> convert chatId
        <> " was successfully changed in db."
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in setRepliesNumber!\n"
            <> show e

{- | Gets a mode for given Chat from the database (table modes).
     If no exists data for given Chat in db then return default mode. -}
getMode :: Handle IO -> ChatId -> IO Text
getMode handle chatId = handleSql errorHandler $ do
  let dbH = hDb handle
      logH = hLogger handle
  -- check if chat_id already in the table:
  r <- quickQuery' dbH "SELECT mode \
                       \FROM modes \
                       \WHERE chat_id = ?"
        [toSql chatId]
  case r of
    [[x]] -> do
      Logger.logInfo logH $ "Will use: "
        <> pack (fromSql x :: String)
        <> " mode for Chat with id: " 
        <> convert chatId
      return $ fromSql x
    _ -> do
      Logger.logInfo logH $ "Will use default mode for Chat with id: "
        <> convert chatId
      return "reply"
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error in getMode!\n"
            <> show e

{- | Sets a mode for given Chat to the database (table modes).
     If no exists data for given Chat in db then sets the given number
     If chatId exists mode in db then change existed mode. -}
setMode :: Handle IO -> ChatId -> Mode -> IO ()
setMode handle chatId mode = handleSql errorHandler $ do
  let dbH = hDb handle
      logH = hLogger handle
  -- check if chat_id already in the table:
  r <- quickQuery' dbH "SELECT mode \
                       \FROM modes \
                       \WHERE chat_id = ?"
        [toSql chatId]
  case r of
    [] -> do
      _ <- run dbH "INSERT INTO modes (chat_id, mode) \
                   \VALUES (?,?)"
            [toSql chatId, toSql mode]
      commit dbH
      Logger.logInfo logH $ "Mode for Chat with id: "
        <> convert chatId
        <> " was successfully inserted in db."
    _ -> do
      _ <- run dbH "UPDATE modes \
                   \SET mode = ? \
                   \WHERE chat_ID = ?"
            [toSql mode, toSql chatId]
      commit dbH
      Logger.logInfo logH $ "Mode for Chat with id: "
        <> convert chatId
        <> " was successfully changed in db."
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error in setMode!\n"
            <> show e
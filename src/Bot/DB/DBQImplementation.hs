{-# LANGUAGE FlexibleContexts #-}

module Bot.DB.DBQImplementation where

import Database.HDBC (handleSql, run, commit, quickQuery', fromSql, toSql)
import qualified Control.Exception as Exc

import qualified Bot.DB.DB as BotDB
import qualified Bot.DB.DBQ as BotDBQ
import qualified Bot.Logger.Logger as Logger
import qualified Bot.Settings as Settings
import qualified Bot.Exception as E
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Mode as BotMode
import qualified Bot.Util as BotUtil

withHandleIO :: Logger.Handle IO ->
                BotDB.Handle IO -> (BotDBQ.Handle IO -> IO a) -> IO a
withHandleIO logger dbH f = do
  let handle = BotDBQ.Handle {
    BotDBQ.hLogger = logger,
    BotDBQ.hDb = dbH,

    BotDBQ.getLastSucUpdate = getLastSucUpdate dbH,
    BotDBQ.putUpdate = putUpdate dbH,
    BotDBQ.getRepliesNumber = getRepliesNumber dbH,
    BotDBQ.setRepliesNumber = setRepliesNumber dbH,
    BotDBQ.getMode = getMode dbH,
    BotDBQ.setMode = setMode dbH
  }
  f handle

{- | Gets id of last successfully processed update from DB -}
getLastSucUpdate :: BotDB.Handle IO -> IO (Maybe BotSynonyms.UpdateId)
getLastSucUpdate handle = handleSql errorHandler $ do
  let dbConn = BotDB.conn handle
      logH = BotDB.hLogger handle
  r <- quickQuery' dbConn "SELECT update_ID \
                          \FROM updates \
                          \ORDER BY update_ID DESC \
                          \LIMIT 1" []
  case r of
    [[x]] -> do
      Logger.logInfo logH $ "Last processed update with id: "
        <> BotUtil.convert (fromSql x :: BotSynonyms.UpdateId)
      return $ Just $ fromSql x
    _ -> do
      Logger.logWarning logH "There are no processed updates for now"
      return Nothing
  where errorHandler _ = do
          Exc.throwIO $ E.DbError "Error: Error in getLastSucUpdate!"

putUpdate :: BotDB.Handle IO -> BotSynonyms.UpdateId -> IO ()
putUpdate handle updateId = handleSql errorHandler $ do
  let dbConn = BotDB.conn handle
      logH = BotDB.hLogger handle
  r <- quickQuery' dbConn "SELECT update_ID \
                          \FROM updates \
                          \WHERE update_ID = ?" 
        [toSql updateId]
  case r of
    [] -> do
      _ <- run dbConn "INSERT INTO updates (update_ID, processed) \
                      \VALUES (?,?)"
            [toSql updateId, toSql True]
      commit dbConn
      Logger.logInfo logH $ "Update with id: " 
        <> BotUtil.convert updateId 
        <> " was successfully inserted in db."
    _ -> Logger.logWarning logH $ "Update with id: "
           <> BotUtil.convert updateId
           <> " already exists in db."
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in putUpdate!\n"
            <> show e

{- | Gets a reply number for given Chat from the database (table rep_numbers).
     If no exists data for given Chat in db then return initial number from config. -}
getRepliesNumber :: BotDB.Handle IO -> BotSynonyms.ChatId -> IO BotSynonyms.RepNum
getRepliesNumber handle chatId = handleSql errorHandler $ do
  let dbConn = BotDB.conn handle
      logH = BotDB.hLogger handle
      config = BotDB.cDb handle
  -- check if chat_id already in the table:
  r <- quickQuery' dbConn "SELECT reply_number \
                          \FROM rep_numbers \
                          \WHERE chat_ID = ?"
        [toSql chatId]
  case r of
    [[x]] -> do
      Logger.logInfo logH $ "Will use: " 
        <> BotUtil.convert (fromSql x :: BotSynonyms.RepNum)
        <> " replies for Chat with id: " 
        <> BotUtil.convert chatId
      return $ fromSql x
    _ -> do
      Logger.logInfo logH 
        $ "Will use initial number of replies for Chat with id: " 
          <> BotUtil.convert chatId
      return $ Settings.botInitialReplyNumber config
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in getRepliesNumber!\n"
            <> show e

{- | Sets a reply number for given Chat to the database (table rep_numbers).
     If no exists data for given Chat in db then sets the given number
     If chatId exists reply_number in db then change existed reply_number. -}
setRepliesNumber :: BotDB.Handle IO -> BotSynonyms.ChatId -> BotSynonyms.RepNum -> IO ()
setRepliesNumber handle chatId repNum = handleSql errorHandler $ do
  let dbConn = BotDB.conn handle
      logH = BotDB.hLogger handle
  -- check if chat_id already in the table:
  r <- quickQuery' dbConn "SELECT reply_number \
                          \FROM rep_numbers \
                          \WHERE chat_ID = ?"
        [toSql chatId]
  case r of
    [] -> do
      _ <- run dbConn "INSERT INTO rep_numbers (chat_id, reply_number) \
                      \VALUES (?,?)"
            [toSql chatId, toSql repNum]
      commit dbConn
      Logger.logInfo logH $ "Reply_number for Chat with id: "
        <> BotUtil.convert chatId 
        <> " was successfully inserted in db."
    _ -> do
      _ <- run dbConn "UPDATE rep_numbers \
                      \SET reply_number = ? \
                      \WHERE chat_id = ?"
            [toSql repNum, toSql chatId]
      commit dbConn
      Logger.logInfo logH $ "Info: Reply_number for Chat with id: "
        <> BotUtil.convert chatId
        <> " was successfully changed in db."
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error: Error in setRepliesNumber!\n"
            <> show e

{- | Gets a mode for given Chat from the database (table modes).
     If no exists data for given Chat in db then return default mode. -}
getMode :: BotDB.Handle IO -> BotSynonyms.ChatId -> IO BotMode.Mode
getMode handle chatId = handleSql errorHandler $ do
  let dbConn = BotDB.conn handle
      logH = BotDB.hLogger handle
  -- check if chat_id already in the table:
  r <- quickQuery' dbConn "SELECT mode \
                          \FROM modes \
                          \WHERE chat_id = ?"
        [toSql chatId]
  case r of
    [[x]] -> do
      Logger.logInfo logH $ "Will use: "
        <> BotUtil.convert x
        <> " mode for Chat with id: " 
        <> BotUtil.convert chatId
      return $ fromSql x
    _ -> do
      Logger.logInfo logH $ "Will use default mode for Chat with id: "
        <> BotUtil.convert chatId
      return BotMode.ReplyMode
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error in getMode!\n"
            <> show e

{- | Sets a mode for given Chat to the database (table modes).
     If no exists data for given Chat in db then sets the given number
     If chatId exists mode in db then change existed mode. -}
setMode :: BotDB.Handle IO -> BotSynonyms.ChatId -> BotMode.Mode -> IO ()
setMode handle chatId mode = handleSql errorHandler $ do
  let dbConn = BotDB.conn handle
      logH = BotDB.hLogger handle
  -- check if chat_id already in the table:
  r <- quickQuery' dbConn "SELECT mode \
                          \FROM modes \
                          \WHERE chat_id = ?"
        [toSql chatId]
  case r of
    [] -> do
      _ <- run dbConn "INSERT INTO modes (chat_id, mode) \
                      \VALUES (?,?)"
            [toSql chatId, toSql mode]
      commit dbConn
      Logger.logInfo logH $ "Mode: " 
        <> BotUtil.convert mode
        <> " for Chat with id: "
        <> BotUtil.convert chatId
        <> " was successfully inserted in db."
    _ -> do
      _ <- run dbConn "UPDATE modes \
                      \SET mode = ? \
                      \WHERE chat_ID = ?"
            [toSql mode, toSql chatId]
      commit dbConn
      Logger.logInfo logH $ "Mode for Chat with id: "
        <> BotUtil.convert chatId
        <> " was successfully changed in db."
  where errorHandler e = do
          Exc.throwIO $ E.DbError $ "Error in setMode!\n"
            <> show e
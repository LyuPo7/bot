module Main where

import qualified Control.Exception as Exc

import Bot.Config
import qualified Bot.DB.DBImplementation as BotDB
import qualified Bot.Exception as E
import qualified Bot.Logger.Logger as Logger
import qualified Bot.Mode.Mode as BotMode
import qualified Bot.Request.RequestImplementation as BotReq
import qualified Bot.System.SystemImplementation as BotSystem

main :: IO ()
main = Exc.handle errorHandler $ do
  config <- getConfig
  let cLog = cLogger config
      cSet = cSettings config
  Logger.withHandleIO cLog $ \hLogger ->
    BotSystem.withHandleIO hLogger cSet $ \hSys ->
      BotDB.withHandleIO hLogger hSys cSet $ \hDb ->
        BotReq.withHandleIO hLogger hDb cSet $ \hReq ->
          BotMode.startMode hReq
 where
  errorHandler :: E.BotError -> IO ()
  errorHandler e = print e

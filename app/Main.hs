module Main where

import qualified Control.Exception as Exc

import Bot.Config
import qualified Bot.Exception as E
import qualified Bot.Settings as Settings
import qualified Bot.Logger as Logger
import qualified Bot.DB.DBQueries as DB
import qualified Bot.Tele.Request.Requests as TReq
import qualified Bot.Tele.Parser.Parser as TParser
import qualified Bot.Tele.Run as Tele
import qualified Bot.Tele.RunSpec as STele
import qualified Bot.Vk.Request.Requests as VReq
import qualified Bot.Vk.Request.Attach as VAttach
import qualified Bot.Vk.Request.Document as VDoc
import qualified Bot.Vk.Parser.Parser as VParser
import qualified Bot.Vk.Run as Vk
import qualified Bot.Vk.RunSpec as SVk

main :: IO ()
main = Exc.handle errorHandler $ do
  -- config
  config <- getConfig
  let cLog = cLogger config
      cSet = cSettings config
      logH = Logger.newHandleIO cLog
      api = Settings.botApi cSet
  case api of
    "telegram" -> do
      Logger.withHandleIO cLog $ \hLogger ->
        DB.withHandleIO hLogger cSet $ \hDb ->
        TParser.withHandleIO hLogger cSet $ \hParser ->
        TReq.withHandleIO hLogger hParser cSet $ \hReq ->
        Tele.withHandleIO hLogger cSet hDb hReq hParser $ \hTele ->
        STele.run hTele
    "vk" -> do
      Logger.withHandleIO cLog $ \hLogger ->
        DB.withHandleIO hLogger cSet $ \hDb ->
        VParser.withHandleIO hLogger $ \hParser ->
        VReq.withHandleIO hLogger cSet hParser $ \hReq ->
        VDoc.withHandleIO hLogger hParser hReq $ \hDoc ->
        VAttach.withHandleIO hLogger hParser hReq hDoc $ \hAttach ->
        Vk.withHandleIO hLogger cSet hDb hReq hParser hAttach $ \hVk ->
        SVk.run hVk
    _ -> do
      Logger.logError logH "Incorrect field 'bot_api' in config.json"
      Exc.throwIO $ E.ParseConfigError "Incorrect field 'bot_api' in config.json" 
    where
      errorHandler :: E.BotError -> IO ()
      errorHandler e = print e
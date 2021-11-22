module Main where

import qualified Control.Exception as Exc

import Bot.Config
import qualified Bot.Exception as E
import qualified Bot.Settings as Settings
import qualified Bot.Logger.Logger as Logger
import qualified Bot.DB.DBImplementation as BotDB
import qualified Bot.DB.DBQImplementation as BotDBQ
import qualified Bot.System.SystemImplementation as BotSystem
import qualified Bot.Mode.Mode as BotMode
import qualified Bot.Api.Tele.Request.Requests as TeleReq
import qualified Bot.Parser.ParserImplementation as BotParserImpl
import qualified Bot.Api.Tele.Mode.Mode as TeleMode
import qualified Bot.Api.Vk.Request.Requests as VkReq
import qualified Bot.Api.Vk.Mode.Mode as VkMode

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
        BotSystem.withHandleIO hLogger cSet $ \hSys ->
        BotDB.withHandleIO hLogger cSet $ \hDbConn ->
        BotDBQ.withHandleIO hLogger hDbConn $ \hDb ->
        BotParserImpl.withHandleIO hLogger hSys cSet $ \hParser ->
        TeleReq.withHandleIO hLogger hDb hParser cSet $ \hReq ->
        TeleMode.withHandleIO hLogger cSet hDb hReq hParser $ \hTele ->
        BotMode.startMode hTele
    "vk" -> do
      Logger.withHandleIO cLog $ \hLogger ->
        BotSystem.withHandleIO hLogger cSet $ \hSys ->
        BotDB.withHandleIO hLogger cSet $ \hDbConn ->
        BotDBQ.withHandleIO hLogger hDbConn $ \hDb ->
        BotParserImpl.withHandleIO hLogger hSys cSet $ \hParser ->
        VkReq.withHandleIO hLogger hDb hParser cSet $ \hReq ->
        VkMode.withHandleIO hLogger cSet hDb hReq hParser $ \hVk ->
        BotMode.startMode hVk
    _ -> do
      Logger.logError logH "Incorrect field 'bot_api' in config.json"
      Exc.throwIO $ E.ParseConfigError "Incorrect field 'bot_api' in config.json" 
    where
      errorHandler :: E.BotError -> IO ()
      errorHandler e = print e
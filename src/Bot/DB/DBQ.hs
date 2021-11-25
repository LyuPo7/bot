module Bot.DB.DBQ where

import Bot.DB.DB as BotDB
import qualified Bot.Logger.Logger as Logger
import qualified Bot.System.System as BotSystem
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Objects.Mode as BotMode
import qualified Bot.Settings as Settings

data Handle m = Handle {
  hLogger :: Logger.Handle m,
  hDb :: BotDB.Handle m,
  hSystem :: BotSystem.Handle m,
  cDBQ :: Settings.Config,

  getLastSucUpdate :: m (Maybe BotSynonyms.UpdateId),
  putUpdate :: BotSynonyms.UpdateId ->
               m (),
  getRepliesNumber :: BotSynonyms.ChatId ->
                      m BotSynonyms.RepNum,
  setRepliesNumber :: BotSynonyms.ChatId ->
                      BotSynonyms.RepNum ->
                      m (),
  getMode :: BotSynonyms.ChatId ->
             m BotMode.Mode,
  setMode :: BotSynonyms.ChatId ->
             BotMode.Mode ->
             m ()
}
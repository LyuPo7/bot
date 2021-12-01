module Bot.Objects.Update where

import qualified Bot.Api.Vk.Objects.Server as VkServer
import qualified Bot.Objects.Synonyms as BotSynonyms

data Update
  = TeleUpdate BotSynonyms.UpdateId
  | VkUpdate VkServer.Server
  deriving (Show)

module Bot.Objects.Update where

import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Api.Vk.Objects.Server as VkServer
import qualified Bot.Api.Vk.Objects.Update as VkUpdate
import qualified Bot.Api.Tele.Objects.Update as TeleUpdate

data Update = TeleUpdate BotSynonyms.UpdateId
            | TeleFullUpdate TeleUpdate.Update
            | VkUpdate VkServer.Server
            | VkFullUpdate VkUpdate.Update
            deriving (Show)
module Bot.Objects.Update where

import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Api.Vk.Objects.Server as VkServer

data Update = TeleUpdate {
                teleUpdate :: BotSynonyms.UpdateId
              }
            | VkUpdate {
                vkUpdate :: VkServer.Server
              }
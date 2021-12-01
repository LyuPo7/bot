module Bot.Objects.FullUpdate where

import qualified Bot.Api.Vk.Objects.Update as VkUpdate
import qualified Bot.Api.Tele.Objects.Update as TeleUpdate

data FullUpdate = TeleUpdate TeleUpdate.Update
                | VkUpdate VkUpdate.Update
                deriving (Show)
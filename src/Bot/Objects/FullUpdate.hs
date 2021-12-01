module Bot.Objects.FullUpdate where

import qualified Bot.Api.Tele.Objects.Update as TeleUpdate
import qualified Bot.Api.Vk.Objects.Update as VkUpdate

data FullUpdate
  = TeleUpdate TeleUpdate.Update
  | VkUpdate VkUpdate.Update
  deriving (Show)

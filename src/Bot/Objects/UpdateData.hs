module Bot.Objects.UpdateData where

import qualified Bot.Api.Tele.Objects.UpdateData as TeleUpData
import qualified Bot.Api.Vk.Objects.UpdateData as VkUpData

data UpdateData
  = TeleUpdateData TeleUpData.UpdateData
  | VkUpdateData VkUpData.UpdateData
  deriving (Show)

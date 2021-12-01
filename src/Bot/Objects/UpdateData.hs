module Bot.Objects.UpdateData where

import qualified Bot.Api.Vk.Objects.UpdateData as VkUpData
import qualified Bot.Api.Tele.Objects.UpdateData as TeleUpData

data UpdateData = TeleUpdateData TeleUpData.UpdateData
                | VkUpdateData VkUpData.UpdateData
                deriving (Show)
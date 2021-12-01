{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Api.Vk.Objects.UpdateData where

import Data.Aeson.Types (FromJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Bot.Api.Vk.Objects.Update as VkUpdate

data UpdateData = UpdateData
  { ts :: Text,
    updates :: [VkUpdate.Update]
  }
  deriving (Show, Read, Eq, Generic, FromJSON)

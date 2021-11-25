{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bot.Objects.Synonyms where

import Data.Aeson.Types (FromJSON, ToJSON)
import Web.Internal.HttpApiData (ToHttpApiData)
import Data.Convertible.Base (Convertible(..), ConvertError(..))
import Database.HDBC (SqlValue(..))

import Data.Text (Text)

newtype Status = Status Bool
  deriving newtype (Show, FromJSON, Read, Ord, Eq)
newtype UserId = UserId Integer
  deriving newtype (Show, FromJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype GroupId = GroupId Integer
  deriving newtype (Show, FromJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype MessageId = MessageId Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype WallId = WallId Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype WallReplyId = WallReplyId Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype PhotoId = PhotoId Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype AudioId = AudioId Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype VideoId = VideoId Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype StickerId = StickerId Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype MarketId = MarketId Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype MarketAlbumId = MarketAlbumId Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype GiftId = GiftId Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype ObjectId = ObjectId Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype DocId = DocId Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype Timeout = Timeout Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype RecordLimit = RecordLimit Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)
newtype Version = Version Text
  deriving newtype (Show, FromJSON, ToJSON, Read, Ord, Eq, ToHttpApiData)
newtype FileType = FileType Text
  deriving newtype (Show, FromJSON, ToJSON, Read, Ord, Eq, ToHttpApiData)
newtype FilePathT = FilePathT Text
  deriving newtype (Show, FromJSON, ToJSON, Read, Ord, Eq, ToHttpApiData)
newtype Host = Host { getHost :: Text }
newtype Url = Url Text
  deriving newtype (Show, FromJSON, ToJSON, Read, Ord, Eq, ToHttpApiData)

instance Convertible Url Text where
  safeConvert (Url url) = Right url

newtype AccessKey = AccessKey Text
  deriving newtype (Show, FromJSON, ToJSON, Read, Ord, Eq, ToHttpApiData)

instance Convertible AccessKey Text where
  safeConvert (AccessKey key) = Right key

newtype Description = Description Text
  deriving newtype (Show, FromJSON, ToJSON, Read, Ord, Eq, ToHttpApiData)

instance Convertible Description Text where
  safeConvert (Description description) = Right description

newtype Token = Token Text
  deriving newtype (Show, FromJSON, ToJSON, Read, Ord, Eq, ToHttpApiData)

instance Convertible Token Text where
  safeConvert (Token token) = Right token

newtype ChatId = ChatId Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)

instance Convertible ChatId SqlValue where
  safeConvert (ChatId chatId) = Right $ SqlInteger chatId

newtype UpdateId = UpdateId Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)

instance Convertible UpdateId SqlValue where
  safeConvert (UpdateId updateId) = Right $ SqlInteger updateId

instance Convertible SqlValue UpdateId where
  safeConvert (SqlInteger updateId) = Right $ UpdateId updateId
  safeConvert (SqlInt64 updateId) = Right $ UpdateId $ toInteger updateId
  safeConvert (SqlInt32 updateId) = Right $ UpdateId $ toInteger updateId
  safeConvert x = Left $ ConvertError {
    convSourceValue = show x,
    convSourceType = "SqlValue",
    convDestType = "UpdateId",
    convErrorMessage = "No Integer/Int64/Int32 Value in field 'update_id'"
  }

newtype RepNum = RepNum Integer
  deriving newtype (Show, FromJSON, ToJSON, Num, Read, Ord, Eq, ToHttpApiData)

instance Convertible RepNum SqlValue where
  safeConvert (RepNum num) = Right $ SqlInteger num

instance Convertible SqlValue RepNum where
  safeConvert (SqlInteger num) = Right $ RepNum num
  safeConvert (SqlInt64 num) = Right $ RepNum $ toInteger num
  safeConvert (SqlInt32 num) = Right $ RepNum $ toInteger num
  safeConvert x = Left $ ConvertError {
    convSourceValue = show x,
    convSourceType = "SqlValue",
    convDestType = "RepNum",
    convErrorMessage = "No Integer/Int64/Int32 Value in field 'reply_number'"
  }


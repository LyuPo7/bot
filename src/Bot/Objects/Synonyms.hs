{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Bot.Objects.Synonyms where

import Data.Aeson.Types (FromJSON, ToJSON)
import Web.Internal.HttpApiData (ToHttpApiData)
import Data.Convertible.Base (Convertible(..))

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

type ChatId = Integer
type UpdateId = Integer
type RepNum = Integer
type Url = Text

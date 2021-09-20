{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Bot.Vk.Request.AttachSpec where

import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Logger as Logger
import Bot.Vk.Parser.Data

data Handle m = Handle {
    hLogger :: Logger.Handle m,
    hReq :: ReqSpec.Handle m,
    hParser :: ParserSpec.Handle m,

    updateDoc :: Document -> m Document
}

updateAttachments :: Monad m => Handle m -> Maybe [Attachment] -> m (Maybe [Attachment])
updateAttachments handle Nothing = do
  let logh = hLogger handle
  Logger.logDebug logh "Message hasn't Attachments."
  return Nothing
updateAttachments handle (Just attachs) = do
  let logh = hLogger handle
  Logger.logDebug logh "Message has Attachments."
  Just <$> mapM (updateAttachment handle) attachs

updateAttachment :: Monad m => Handle m -> Attachment -> m Attachment
updateAttachment handle attach = do
  let attachType = attach_type attach
  case attachType of
    "doc" -> do
      case attach_doc attach of
        Nothing -> return attach
        Just doc -> do
          docNew <- updateDoc handle doc
          return attach { attach_doc = Just docNew }
    _ -> return attach
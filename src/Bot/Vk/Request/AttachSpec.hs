module Bot.Vk.Request.AttachSpec where

import qualified Bot.Vk.Parser.ParserSpec as ParserSpec
import qualified Bot.Vk.Request.RequestsSpec as ReqSpec
import qualified Bot.Logger as Logger
import Bot.Vk.Parser.Objects.Document (Document(..))
import Bot.Vk.Parser.Objects.Attachment (Attachment(..))

data Handle m = Handle {
  hLogger :: Logger.Handle m,
  hReq :: ReqSpec.Handle m,
  hParser :: ParserSpec.Handle m,

  updateDoc :: Document -> m Document
}

updateAttachments :: Monad m => Handle m ->
                     Maybe [Attachment] -> m (Maybe [Attachment])
updateAttachments handle Nothing = do
  let logH = hLogger handle
  Logger.logDebug logH "Message hasn't Attachments."
  return Nothing
updateAttachments handle (Just attachs) = do
  let logH = hLogger handle
  Logger.logDebug logH "Message has Attachments."
  Just <$> mapM (updateAttachment handle) attachs

updateAttachment :: Monad m => Handle m -> Attachment -> m Attachment
updateAttachment handle attach = do
  case attach of
    AttachDoc doc -> do
      docNew <- updateDoc handle doc
      return $ AttachDoc docNew
    _ -> return attach
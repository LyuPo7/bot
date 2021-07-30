{-# LANGUAGE OverloadedStrings #-}

module Bot.Tele.Request.Requests where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.Text (Text, unpack)
import GHC.Generics ()
import Data.Aeson (encode)
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseStatus, responseBody, RequestBody(..), Request(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode, statusCode)

import qualified Bot.Config as Config
import qualified Bot.Logger as BL
import Bot.Settings (apiTele, config, getHost)
import Bot.Tele.Request.Data

{-- | Request to api --}
makeRequest :: BL.Handle -> TeleRequest -> RequestOptions -> IO B.ByteString
makeRequest logh teleRequest options = do
          manager <- newManager tlsManagerSettings
          token <- fmap Config.botToken config
          let hostApi = BC.unpack $ getHost apiTele
          let methodApi = BC.unpack $ getRequest teleRequest
          let api = concat [hostApi, unpack token, methodApi]
          initialRequest <- parseRequest api
          let request =
                        initialRequest
                                  { method = "POST"
                                  , requestBody =
                                              RequestBodyLBS $ encode options
                                  , requestHeaders =
                                              [ ( "Content-Type"
                                                , "application/json; charset=utf-8")
                                              ]
                                  }
          response <- httpLbs request manager
          BL.logDebug logh $
                    "The status code was: " ++
                    show (statusCode $ responseStatus response)
          return $ responseBody response

{-- | Get update request --}
getUpdate :: BL.Handle -> Maybe Integer -> IO L8.ByteString
getUpdate logh updateId  = do
  -- Get JSON data
          let updateOptions =
                        GetUpdates
                                  { updates_offset = updateId
                                  , updates_limit = Nothing
                                  , updates_timeout = Nothing
                                  , updates_allowedUpdates = Just ["message"]
                                  }
          makeRequest logh getUpdates updateOptions

{-- | Send text Message request --}
sendTextMessage :: BL.Handle -> Integer -> Text -> IO ()
sendTextMessage logh chatId text = do
          let message =
                        SendMessage
                                  { sendMes_chatId = chatId
                                  , sendMes_text = text
                                  , sendMes_disableNotification = Nothing
                                  , sendMes_replyToMessageId = Nothing
                                  }
          _ <- makeRequest logh sendMessage message
          BL.logInfo logh $
                    "Message with text: " ++
                    show text ++ " was sended to chat with id: " ++ show chatId

{-- | Copy Message request --}
sendEchoMessage :: BL.Handle -> Integer -> Integer -> IO ()
sendEchoMessage logh chatId messageId = do
          let message =
                        CopyMessage
                                  { copwMes_chatId = chatId
                                  , copwMes_fromChatId = chatId
                                  , copwMes_messageId = messageId
                                  }
          _ <- makeRequest logh copyMessage message
          BL.logInfo logh $
                    "Echo-Message with id: " ++
                    show messageId ++
                    " was forwarded to chat with id: " ++ show chatId

{-- | Copy(n times) Message request --}
sendNEchoMessage :: BL.Handle -> Integer -> Integer -> Integer -> IO ()
sendNEchoMessage logh _ _ 0 =
          BL.logInfo logh "Echo-Messages were sended."
sendNEchoMessage logh chatId messageId n = do
          sendEchoMessage logh chatId messageId
          sendNEchoMessage logh chatId messageId (n - 1)

{-- | Send Message with InlineKeyboard --}
sendQueryNumber :: BL.Handle -> Integer -> Text -> IO L8.ByteString
sendQueryNumber logh chatId question = do
          let b1 =
                        InlineKeyboardButton
                                  { inlineButton_text = "1"
                                  , inlineButton_callbackData = Just "Pressed 1"
                                  }
          let b2 =
                        InlineKeyboardButton
                                  { inlineButton_text = "2"
                                  , inlineButton_callbackData = Just "Pressed 2"
                                  }
          let b3 =
                        InlineKeyboardButton
                                  { inlineButton_text = "3"
                                  , inlineButton_callbackData = Just "Pressed 3"
                                  }
          let b4 =
                        InlineKeyboardButton
                                  { inlineButton_text = "4"
                                  , inlineButton_callbackData = Just "Pressed 4"
                                  }
          let b5 =
                        InlineKeyboardButton
                                  { inlineButton_text = "5"
                                  , inlineButton_callbackData = Just "Pressed 5"
                                  }
          let markupIn =
                        InlineKeyboardMarkup
                                  { inlineMarkup_keyboard =
                                              [[b1, b2, b3, b4, b5]]
                                  , inlineMarkup_oneTimeKeyboard = Just True
                                  , inlineMarkup_resizeKeyboard = Just True
                                  }
          let query =
                        QueryMessage
                                  { sendQue_chatId = chatId
                                  , sendQue_text = question
                                  , sendQue_disableNotification = Nothing
                                  , sendQue_replyToMessageId = Nothing
                                  , sendQue_replyMarkup = Just markupIn
                                  }
          BL.logInfo logh $ "Question was sended to chat with id: " ++ show chatId
          makeRequest logh sendMessage query

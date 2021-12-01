module TestBot.Request.Request where

import Data.Convertible.Base (convert)

import qualified Hedgehog.Gen as Gen
import Test.Hspec (Spec, describe, it, shouldBe)

import qualified TestBot.Api.Tele.GenData as TeleGD
import qualified TestBot.Api.Tele.Handlers as TeleHandlers
import qualified TestBot.Api.Vk.Handlers as VkHandlers
import qualified TestBot.GenData as BotGD

import qualified Bot.Api.Tele.Objects.Chat as TeleChat
import qualified Bot.Api.Tele.Objects.Message as TeleMessage
import qualified Bot.Api.Tele.Objects.MessageEntity as TeleMessageEntity
import qualified Bot.Api.Tele.Objects.Method as TeleMethod
import qualified Bot.Api.Tele.Objects.RequestOptions as TeleReqOptions
import qualified Bot.Api.Tele.Objects.SendMessage as TeleSendMessage
import qualified Bot.Api.Vk.Objects.Attachment as VkAttach
import qualified Bot.Api.Vk.Objects.Document as VkDoc
import qualified Bot.Api.Vk.Objects.Message as VkMessage
import qualified Bot.Api.Vk.Objects.Method as VkMethod
import qualified Bot.Api.Vk.Objects.RequestOptions as VkReqOptions
import qualified Bot.Objects.Document as BotDoc
import qualified Bot.Objects.Message as BotMessage
import qualified Bot.Objects.RequestPair as BotReqPair
import qualified Bot.Objects.Synonyms as BotSynonyms
import qualified Bot.Request.Request as BotReq
import qualified Bot.Settings as Settings

spec_getServer :: Spec
spec_getServer =
  describe "Testing getServer" $ do
    it "Should return ByteString containing server info" $ do
      let result = BotReq.getServer VkHandlers.reqH
          check = "ok"
      result `shouldBe` Just check
    it "Should fail if Method is missing" $ do
      let result = BotReq.getServer TeleHandlers.reqH
      result `shouldBe` Nothing

spec_getUploadedServer :: Spec
spec_getUploadedServer =
  describe "Testing getUploadedServer" $ do
    it "Should return ByteString containing uploaded server info" $ do
      botDoc <- Gen.sample BotGD.genBotDoc
      let result = BotReq.getUploadedServer VkHandlers.reqH botDoc
          check = "ok"
      result `shouldBe` Just check
    it "Should fail if Method is missing" $ do
      botDoc <- Gen.sample BotGD.genBotDoc
      let result = BotReq.getUploadedServer TeleHandlers.reqH botDoc
      result `shouldBe` Nothing

spec_sendEchoMessage :: Spec
spec_sendEchoMessage =
  describe "Testing sendEchoMessage" $ do
    it "Should return the same message if Api doesn't require update message" $ do
      message <- Gen.sample TeleGD.genMessage
      let botMessage = BotMessage.TeleMessage message
          result = BotReq.sendEchoMessage TeleHandlers.reqH botMessage
      result `shouldBe` Just botMessage

spec_sendHelpMessage :: Spec
spec_sendHelpMessage =
  describe "Testing sendHelpMessage" $ do
    it "Should return type and method for Api supporting JSON-content-type" $ do
      let teleChat =
            TeleChat.Chat
              { TeleChat.id = 222
              }
          teleEntities =
            TeleMessageEntity.MessageEntity
              { TeleMessageEntity.entity_type = "bot_command"
              }
          message =
            TeleMessage.Message
              { TeleMessage.message_id = 101,
                TeleMessage.chat = teleChat,
                TeleMessage.text = Just "Hi!",
                TeleMessage.entities = Just [teleEntities]
              }
          botMessage = BotMessage.TeleMessage message
          result = BotReq.sendHelpMessage TeleHandlers.reqH botMessage
          options =
            TeleReqOptions.SendMessage $
              TeleSendMessage.SendMessage
                { TeleSendMessage.chat_id = 222,
                  TeleSendMessage.text =
                    convert $
                      Settings.botDescription
                        TeleHandlers.runC,
                  TeleSendMessage.disable_notification = Nothing,
                  TeleSendMessage.reply_to_message_id = Nothing
                }
          method = TeleMethod.sendMessage
          pair = BotReqPair.TeleReqPair (method, options)
      result `shouldBe` Just pair
    it "Should return string and method for Api supporting JSON-content-type" $ do
      let doc =
            VkDoc.Document
              { VkDoc.id = 781,
                VkDoc.owner_id = 129,
                VkDoc.title = "book.pdf",
                VkDoc.url = BotSynonyms.Url "https://server/link/222",
                VkDoc.access_key = BotSynonyms.AccessKey "x\n 0\n \NAK^IMYz.<E"
              }
          docAttach = VkAttach.AttachDoc doc
          attachs = Just [docAttach]
          message =
            VkMessage.Message
              { VkMessage.message_id = Just 33,
                VkMessage.user_id = 21,
                VkMessage.body = "/help",
                VkMessage.geo = Nothing,
                VkMessage.attachments = attachs,
                VkMessage.fwd_messages = Nothing
              }
          botMessage = BotMessage.VkMessage message
          result = BotReq.sendHelpMessage VkHandlers.reqH botMessage
          options =
            VkReqOptions.RequestOptions
              "access_token=abcd0dcba&\
              \message=Hi%21%20I%27m%20bot%3D%29&\
              \user_id=21&\
              \v=5.81"
          method = VkMethod.sendMessage
          pair = BotReqPair.VkReqPair (method, options)
      result `shouldBe` Just pair

spec_saveUploadedDoc :: Spec
spec_saveUploadedDoc =
  describe "Testing saveUploadedDoc" $ do
    it "Should return ByteString containing uploaded server info" $ do
      let result = BotReq.saveUploadedDoc VkHandlers.reqH "text"
          check = "ok"
      result `shouldBe` Just check
    it "Should fail if Method is missing" $ do
      let result = BotReq.saveUploadedDoc TeleHandlers.reqH "text"
      result `shouldBe` Nothing

spec_updateMessage :: Spec
spec_updateMessage =
  describe "Testing updateMessage" $ do
    it "Should return the same message if Api doesn't require change anything in message" $ do
      let message =
            VkMessage.Message
              { VkMessage.message_id = Just 33,
                VkMessage.user_id = 21,
                VkMessage.body = "Hi!",
                VkMessage.geo = Nothing,
                VkMessage.attachments = Nothing,
                VkMessage.fwd_messages = Nothing
              }
          botMessage = BotMessage.VkMessage message
          result = BotReq.updateMessage VkHandlers.reqH botMessage
      result `shouldBe` Just botMessage

spec_updateDoc :: Spec
spec_updateDoc =
  describe "Testing updateDoc" $ do
    it "Should fail if Api doesn't require change anything in message" $ do
      let doc =
            VkDoc.Document
              { VkDoc.id = 781,
                VkDoc.owner_id = 129,
                VkDoc.title = "book.pdf",
                VkDoc.url = BotSynonyms.Url "https://server/link/222",
                VkDoc.access_key = BotSynonyms.AccessKey "x\n 0\n \NAK^IMYz.<E"
              }
          botDoc = BotDoc.VkDoc doc
          result = BotReq.updateDoc TeleHandlers.reqH botDoc
      result `shouldBe` Nothing

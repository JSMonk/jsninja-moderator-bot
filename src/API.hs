{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module API
  ( deleteMessage
  , banUser
  ) where

import Data.Aeson
import Data.Coerce (coerce, Coercible)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Proxy
import GHC.Int (Int32)
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)
import Telegram.Bot.API
import Telegram.Bot.API.Internal.Utils

data DeleteMessageRequest = DeleteMessageRequest 
  { deleteMessageChatId :: ChatId
  , deleteMessageMessageId :: MessageId 
  } deriving (Generic)

instance ToJSON   DeleteMessageRequest where toJSON = gtoJSON
instance FromJSON DeleteMessageRequest where parseJSON = gparseJSON

data KickChatMemberRequest = KickChatMemberRequest
  { kickMessageChatId :: ChatId
  , kickMessageUserId :: UserId 
  } deriving (Generic)

instance ToJSON   KickChatMemberRequest where toJSON = gtoJSON
instance FromJSON KickChatMemberRequest where parseJSON = gparseJSON

type DeleteMessage = "deleteMessage" 
  :> ReqBody '[JSON] DeleteMessageRequest
  :> Post '[JSON] (Response Bool)

type BanUser = "kickChatMember"
  :> ReqBody '[JSON] KickChatMemberRequest
  :> Post '[JSON] (Response Bool)

deleteMessage :: Update -> ClientM (Response Bool)
deleteMessage upd = 
  client (Proxy @DeleteMessage) requestData 
  where
    requestData = DeleteMessageRequest { deleteMessageChatId = chatId'
                                       , deleteMessageMessageId = messageId' } 
    chatId' =  fromJust $ chatId . messageChat <$> msg
    messageId' =  fromJust $ messageMessageId <$> msg
    msg = updateMessage upd

banUser :: Update -> ClientM (Response Bool)
banUser upd =
  client (Proxy @BanUser) $ requestData
  where
    requestData = KickChatMemberRequest { kickMessageChatId = chatId'
                                        , kickMessageUserId = userId' }
    chatId' = fromJust $ chatId . messageChat <$> msg
    userId' = fromJust $ userId <$> messageFrom'
    messageFrom' = msg >>= messageFrom
    msg = updateMessage upd

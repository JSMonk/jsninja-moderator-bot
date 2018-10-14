{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module API
  ( deleteMessage
  , banUser
  ) where

import Data.Coerce (coerce)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Proxy
import GHC.Int (Int32)
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)
import Telegram.Bot.API

type DeleteMessage = "deleteMessage" 
  :> QueryParam "chat_id" Integer
  :> QueryParam "message_id" Int32
  :> Get '[JSON] (Response Bool)

type BanUser = "kickChatMember"
  :> QueryParam "chat_id" Integer
  :> QueryParam "user_id" Int32
  :> Get '[JSON] (Response Bool)

deleteMessage :: Update -> ClientM (Response Bool)
deleteMessage upd = 
  client (Proxy @DeleteMessage) chatId' messageId'
  where
    chatId' =  coerce . chatId . messageChat <$> msg
    messageId' =  coerce . messageMessageId <$> msg
    msg = updateMessage upd

banUser :: Update -> ClientM (Response Bool)
banUser upd =
  client (Proxy @BanUser) chatId' userId'
  where
    chatId' = coerce . chatId . messageChat <$> msg
    userId' = coerce . userId <$> messageFrom'
    messageFrom' = msg >>= messageFrom
    msg = updateMessage upd

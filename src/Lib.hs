{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( serveBot
    ) where

import Control.Monad (void)
import API (deleteMessage, banUser)
import Data.Maybe (isJust, fromJust, isNothing)
import Data.Text (Text, unpack, pack)
import Heroku (runHerokuPing)
import Servant.Client (ClientM)
import System.Environment (getEnv)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.API.MakingRequests (botBaseUrl)
import Telegram.Bot.Simple.Eff (liftClientM)
import Telegram.Bot.Simple.UpdateParser
import Text.Regex.PCRE

urlTemplate :: String
urlTemplate = "(https?:\\/\\/)?(t(elegram)?\\.me|telegram\\.org)\\/([\\w\\_]{5,32})\\/?"

telegramAPI :: String
telegramAPI = "https://api.telegram.org/bot"

type Model = ()

data Action
  = NoOp
  | ShouldBeDeleted Update
  | ReactOnMessage Update 

checkOnLinkExisting :: String -> Bool
checkOnLinkExisting s = s =~ urlTemplate;

hasLinkInside :: Maybe Message -> (Message -> Maybe Text) -> Bool
hasLinkInside msg getter = 
  isJust hasLink && fromJust hasLink 
  where
    getterText = msg >>= getter 
    hasLink = checkOnLinkExisting <$> unpack <$> getterText
   
isRedirectedFromChat :: Maybe Message -> Bool
isRedirectedFromChat msg = 
  isJust chat && (isNothing isNotOurNewsChat || fromJust isNotOurNewsChat)
  where 
    chat = msg >>= messageForwardFromChat
    isNotOurNewsChat = ((/=) "jsninja_news") <$> (chat >>= chatUsername)

isSpam :: Update -> Bool
isSpam upd = 
  (hasLinkInside m messageText || hasLinkInside m messageCaption)
  && isRedirectedFromChat m
  where
     m = updateMessage upd 

moderator :: BotApp Model Action
moderator = BotApp
  { botInitialModel = ()
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ = Just . ReactOnMessage

fetch :: ClientM a -> BotM ()
fetch a = void $ liftClientM $ a

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoOp -> pure model
  ShouldBeDeleted msg -> model <# do
    fetch $ deleteMessage msg
    fetch $ banUser msg
    return NoOp
  ReactOnMessage msg -> model <# do
      return action
    where
      action = if isSpam msg  
               then ShouldBeDeleted msg
               else NoOp

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId moderator) env

serveBot :: IO ()
serveBot = do
  putStrLn "Moderator bot started"
  token <- Token . pack <$> getEnv "TELEGRAM_TOKEN"
  port <- getEnv "PORT"
  runHerokuPing port
  run token

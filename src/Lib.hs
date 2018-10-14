{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( serveBot
    ) where
import Control.Monad (void)
import Control.Concurrent.Async (async)
import API (deleteMessage, banUser)
import Data.Maybe (isJust, fromJust, isNothing)
import Data.Text (Text, unpack, pack)
import Servant.Client (ClientM)
import System.Environment (getEnv)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Text.Regex.PCRE
import Heroku (serveHerokuPingServer)

urlTemplate :: String
urlTemplate = "(https?:\\/\\/)?(t(elegram)?\\.me|telegram\\.org)\\/([\\w\\_]{5,32})\\/?"

greatingTemplate :: String
greatingTemplate = "будет жить. Поприветствуем!"

telegramAPI :: String
telegramAPI = "https://api.telegram.org/bot"

type Model = ()

data Action
  = NoOp
  | ShouldBeDeleted Update
  | ShouldBeBanned Update
  | ReactOnMessage Update 

checkOnLinkExisting :: String -> Bool
checkOnLinkExisting s = s =~ urlTemplate

isGreatings :: String -> Bool
isGreatings s = s =~ greatingTemplate

terminatorEvidence :: User -> Bool
terminatorEvidence usr = 
  userIsBot usr && userName' == "Cyberdyne_Systems_bot"
  where
    userName' = fromJust . userUsername $ usr

isTerminatorGreatings :: Update -> Bool
isTerminatorGreatings upd =
  isTerminatorBot && isGreatings txt
  where
    isTerminatorBot = isJust maybeTerminatorBot && fromJust maybeTerminatorBot
    maybeTerminatorBot = terminatorEvidence <$> sender
    sender = message >>= messageFrom 
    txt = unpack . fromJust $ message >>= messageText
    message = updateMessage upd 

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
  ShouldBeBanned msg -> model <# do
    fetch $ banUser msg
    return NoOp
  ShouldBeDeleted msg -> model <# do
    fetch $ deleteMessage msg
    return action
    where 
      action = if isSpam msg
               then ShouldBeBanned msg
               else NoOp
  ReactOnMessage msg -> model <# do
      return action
    where
      action = if isSpam msg || isTerminatorGreatings msg 
               then ShouldBeDeleted msg
               else NoOp

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId moderator) env

serveBot :: IO ()
serveBot = do
  async serveHerokuPingServer 
  putStrLn "Moderator bot started"
  token <- Token . pack <$> getEnv "TELEGRAM_TOKEN"
  run token

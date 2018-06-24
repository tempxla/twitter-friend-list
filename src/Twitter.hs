{-# LANGUAGE DeriveGeneric #-}

module Twitter
  ( getUserList
  , getUserId
  , getScreenName
  , requestTwitter
  ) where

import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Char8     as BS
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status (statusCode)
import           Parser
import           Types
import           Utils
import           Web.Authenticate.OAuth

data FollowerList = FollowerList
  { users           :: [Follower]
  , next_cursor_str :: String
  } deriving (Show, Generic)

instance FromJSON FollowerList

data Follower = Follower
  { id_str      :: String
  , screen_name :: String
  , following   :: Bool
  } deriving (Show, Generic)

instance FromJSON Follower

type SignedManager = (Manager, OAuth, Credential)

mkOAuth :: TwitApiKeys -> OAuth
mkOAuth k = newOAuth
  { oauthServerName     = "api.twitter.com"
  , oauthConsumerKey    = BS.pack $ consumerKey k
  , oauthConsumerSecret = BS.pack $ consumerSecret k
  }

mkCredential :: TwitApiKeys -> Credential
mkCredential k = newCredential (BS.pack $ accessToken k) (BS.pack $ accessTokenSecret k)

mkSignedManager :: EO SignedManager
mkSignedManager = do
  key <- liftEither =<< liftIO readTwitApiKeys
  manager <- liftIO $ newManager tlsManagerSettings
  return (manager, mkOAuth key, mkCredential key)

getResponse :: FromJSON a => SignedManager -> String -> EO a
getResponse (man, oth, cred) url = do
  resp <- flip httpLbs man =<< signOAuth oth cred =<< parseRequest url
  case statusCode (responseStatus resp) of
    200  -> liftEither $ eitherDecode $ responseBody resp
    _    -> throwError $ show (responseStatus resp) ++ "\n" ++
                         either id showValue (eitherDecode $ responseBody resp)

getFollowerList :: SignedManager -> String -> EO FollowerList
getFollowerList man cur = getResponse man $
  "https://api.twitter.com/1.1/followers/list.json?count=200&cursor=" ++ cur

getFriendsList :: SignedManager -> String -> EO FollowerList
getFriendsList man cur = getResponse man $
  "https://api.twitter.com/1.1/friends/list.json?count=200&cursor=" ++ cur

getFollowerListAll :: (String -> EO FollowerList) -> EO [Follower]
getFollowerListAll getList = concat <$> f "-1"
  where
    f "0" = return []
    f cur = getList cur >>= \list -> (users list :) <$> f (next_cursor_str list)

--
-- Return: (Follower, Following)
--
getUserList :: EO ([User], [User])
getUserList = do
  man <- mkSignedManager
  wer <- map followerToUser <$> getFollowerListAll (getFollowerList man)
  ing <- map (followingToUser wer) <$> getFollowerListAll (getFriendsList man)
  return (wer, ing)
    where
      followerToUser :: Follower -> User
      followerToUser x = User
        { idStr      = id_str       x
        , screenName = screen_name  x
        , friendShip = if following x then Friend else FollowedBy
        }
      followingToUser :: [User] -> Follower -> User
      followingToUser ws x = User
        { idStr      = id_str      x
        , screenName = screen_name x
        , friendShip = nullIf Following (const Friend) $ filter (\w -> id_str x == idStr w) ws
        }

getUserId :: String -> EO String
getUserId sname = do
  man <- mkSignedManager
  let url = "https://api.twitter.com/1.1/users/show.json?screen_name=" ++ sname
  id_str <$> getResponse man url

getScreenName :: String -> EO String
getScreenName uid = do
  man <- mkSignedManager
  let url = "https://api.twitter.com/1.1/users/show.json?user_id=" ++ uid
  screen_name <$> getResponse man url

requestTwitter :: String -> EO Value
requestTwitter url = do
  man <- mkSignedManager
  getResponse man url

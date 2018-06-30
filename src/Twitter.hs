{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Twitter
  ( getUserList
  , getUserId
  , getScreenName
  , requestTwitter
  ) where

import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Char8     as BS
import qualified Data.Text                 as T
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status (statusCode)
import           Parser
import           Types
import           Utils
import           Web.Authenticate.OAuth

data FollowerList = FollowerList
  { users           :: [Follower]
  , next_cursor_str :: T.Text
  } deriving (Show, Generic)

instance FromJSON FollowerList

data Follower = Follower
  { id_str      :: T.Text
  , screen_name :: T.Text
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
  key <- readTwitApiKeys
  manager <- liftIO $ newManager tlsManagerSettings
  return (manager, mkOAuth key, mkCredential key)

getResponse :: FromJSON a => SignedManager -> T.Text -> EO a
getResponse (man, oth, cred) url = do
  resp <- flip httpLbs man =<< signOAuth oth cred =<< parseRequest (T.unpack url)
  case statusCode (responseStatus resp) of
    200  -> liftEither $ mapLeft T.pack $ eitherDecode $ responseBody resp
    _    -> throwError $ tshow (responseStatus resp) ＋ "\n" ＋
                         either T.pack showValue (eitherDecode $ responseBody resp)

getFollowerList :: SignedManager -> T.Text -> EO FollowerList
getFollowerList man cur = getResponse man $
  "https://api.twitter.com/1.1/followers/list.json?count=200&cursor=" ＋ cur

getFriendsList :: SignedManager -> T.Text -> EO FollowerList
getFriendsList man cur = getResponse man $
  "https://api.twitter.com/1.1/friends/list.json?count=200&cursor=" ＋ cur

getFollowerListAll :: (T.Text -> EO FollowerList) -> EO [Follower]
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

getUserId :: T.Text -> EO T.Text
getUserId sname = do
  man <- mkSignedManager
  let url = "https://api.twitter.com/1.1/users/show.json?screen_name=" ＋ sname
  id_str <$> getResponse man url

getScreenName :: T.Text -> EO T.Text
getScreenName uid = do
  man <- mkSignedManager
  let url = "https://api.twitter.com/1.1/users/show.json?user_id=" ＋ uid
  screen_name <$> getResponse man url

requestTwitter :: T.Text -> EO Value
requestTwitter url = do
  man <- mkSignedManager
  getResponse man url

{-# LANGUAGE DeriveGeneric #-}

module Twitter
  ( getUserList
  , getUserId
  , getScreenName
  ) where

import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Char8  as BS
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.HTTP.Simple    (getResponseStatusCode)
import           Parser
import           Types
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

getResponse :: FromJSON a => SignedManager -> Request -> EO a
getResponse (man, oth, cred) req = do
  sReq <- signOAuth oth cred req
  resp <- httpLbs sReq man
  case getResponseStatusCode resp of
    200  -> liftEither $ eitherDecode $ responseBody resp
    code -> throwError $ show code ++ "  " ++ show (responseBody resp)

getFollowerList :: SignedManager -> String -> String -> EO FollowerList
getFollowerList sman which cur = getResponse sman =<< parseRequest url
  where
    url = "https://api.twitter.com/1.1/" ++ which ++ "/list.json?count=200&cursor=" ++ cur

getFollowerListAll :: SignedManager -> String -> EO [Follower]
getFollowerListAll man which = concat <$> f "-1"
  where
    f "0" = return []
    f cur = do
      list <- getFollowerList man which cur
      (users list :) <$> f (next_cursor_str list)

--
-- Return: (Follower, Following)
--
getUserList :: EO ([User], [User])
getUserList = do
  sman <- mkSignedManager
  wer <- map followerToUser <$> getFollowerListAll sman "followers"
  ing <- map (followingToUser wer) <$> getFollowerListAll sman "friends"
  return (wer, ing)

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
  , friendShip = case filter (\w -> id_str x == idStr w) ws of
                   [] -> Following
                   _  -> Friend
  }

getUserInfo :: SignedManager -> Maybe String -> Maybe String -> EO Follower
getUserInfo sman uid sname = getResponse sman =<< parseRequest url
  where
    urlBase = "https://api.twitter.com/1.1/users/show.json"
    url = urlBase ++ maybe (maybe "" ("?screen_name="++) sname) ("?user_id="++) uid

getUserId :: String -> EO String
getUserId sname = do
  sman <- mkSignedManager
  id_str <$> getUserInfo sman Nothing (Just sname)

getScreenName :: String -> EO String
getScreenName uid = do
  sman <- mkSignedManager
  screen_name <$> getUserInfo sman (Just uid) Nothing

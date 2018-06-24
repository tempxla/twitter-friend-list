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
import qualified Data.ByteString.Char8  as BS
import qualified Data.HashMap.Strict    as M
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.HTTP.Simple    (getResponseStatusCode)
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
  case getResponseStatusCode resp of
    200  -> liftEither $ eitherDecode $ responseBody resp
    code -> throwError $ show code ++ "  " ++ show (responseBody resp)

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

requestTwitter :: String -> EO String
requestTwitter url = do
  (man, oth, cred) <- mkSignedManager
  resp <- flip httpLbs man =<< signOAuth oth cred =<< parseRequest url
  case getResponseStatusCode resp of
    200  -> liftEither . fmap pretty $ eitherDecode $ responseBody resp
    code -> throwError $ show code ++ "\n" ++
                         show (fmap pretty (eitherDecode $ responseBody resp))

pretty :: Value -> String
pretty v = case v of
  (Object o) -> objf 0 o
  (Array o)  -> arrf 0 o
  o          -> sf o
  where
    obj n (t, Object o) acc = nest n ++ T.unpack t ++ " : " ++ objf n o ++ acc
    obj n (t, Array o)  acc = nest n ++ T.unpack t ++ " : " ++ arrf n o ++ acc
    obj n (t, o)        acc = nest n ++ T.unpack t ++ " : " ++ sf o ++ "\n" ++ acc
    arr n (Object o)    acc = nest n ++ objf n o ++ acc
    arr n (Array o)     acc = nest n ++ arrf n o ++ acc
    arr n o             acc = nest n ++ sf o ++ "\n" ++ acc
    objf n            = paren n '{' obj . M.toList
    arrf n            = paren n '[' arr . V.toList
    nest n            = replicate (n * 2) ' '
    paren n p f = (p:) . (++ q:"\n") . nullIf "" (('\n':) . (++ nest n) . foldr (f (n+1)) "")
      where q = if p == '[' then ']' else '}'
    sf (String o) = "String \"" ++ T.unpack o ++ "\""
    sf o          = show o

{-# LANGUAGE DeriveGeneric #-}

module Twitter
  ( getUserList
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
instance ToJSON FollowerList

data Follower = Follower
  { id_str      :: String
  , screen_name :: String
  , following   :: Bool
  } deriving (Show, Generic)

instance FromJSON Follower
instance ToJSON Follower

type SignedManager = (Manager, OAuth, Credential)

mkOAuth :: TwitApiKeys -> OAuth
mkOAuth k = newOAuth
  { oauthServerName     = "api.twitter.com"
  , oauthConsumerKey    = BS.pack $ consumerKey k
  , oauthConsumerSecret = BS.pack $ consumerSecret k
  }

mkCredential :: TwitApiKeys -> Credential
mkCredential k = newCredential (BS.pack $ accessToken k) (BS.pack $ accessTokenSecret k)

getFollowerList :: SignedManager -> String -> String -> EO FollowerList
getFollowerList (man, oth, cred) which cur = do
  req <- parseRequest
    $ "https://api.twitter.com/1.1/" ++ which ++ "/list.json?count=200&cursor=" ++ cur
  sReq <- signOAuth oth cred req
  resp <- httpLbs sReq man
  case getResponseStatusCode resp of
    200  -> liftEither $ eitherDecode $ responseBody resp
    code -> throwError $ show code ++ "  " ++ show (responseBody resp)

getFollowerListAll :: SignedManager -> String -> EO [Follower]
getFollowerListAll man which = concat <$> f "-1"
  where
    f "0" = return []
    f cur = do
      list <- getFollowerList man which cur
      (users list :) <$> f (next_cursor_str list)

--
-- Return: (Follower, Follower)
--
getUserList :: EO ([User], [User])
getUserList = do
  key <- liftEither =<< liftIO readTwitApiKeys
  manager <- liftIO $ newManager tlsManagerSettings
  let sman = (manager, mkOAuth key, mkCredential key)
  wer <- map followerToUser <$> getFollowerListAll sman "followers"
  ing <- map (followingToUser wer) <$> getFollowerListAll sman "friends"
  return (wer, ing)

followerToUser :: Follower -> User
followerToUser x = User
  { idStr         = id_str x
  , screenName = screen_name x
  , friendShip = if following x then Friend else Followered
  }

followingToUser :: [User] -> Follower -> User
followingToUser ys x = User
  { idStr         = id_str x
  , screenName = screen_name x
  , friendShip = case filter (\y -> id_str x == idStr y) ys of
                   [] -> Following
                   _  -> Friend
  }

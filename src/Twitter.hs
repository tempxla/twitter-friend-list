{-# LANGUAGE DeriveGeneric #-}

module Twitter
  ( getUserList
  , getUserId
  , getScreenName
  , requestTwitter
  , tweet
  , addListMember
  ) where

import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Char8     as BS
import qualified Data.Text                 as T
import           Data.Text.Encoding
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status (statusCode)
import           Parser
import           Types
import           Utils
import           Web.Authenticate.OAuth

data TwUserList = TwUserList
  { users           :: [TwUser]
  , next_cursor_str :: String
  } deriving (Show, Generic)

instance FromJSON TwUserList

data TwUser = TwUser
  { id_str      :: String
  , screen_name :: String
  , following   :: Bool
  } deriving (Show, Generic)

instance FromJSON TwUser

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

httpGET :: FromJSON a => SignedManager -> String -> EO a
httpGET (man, oth, cred) url = do
  resp <- flip httpLbs man =<< signOAuth oth cred =<< parseRequest url
  case statusCode (responseStatus resp) of
    200  -> liftEither $ eitherDecode $ responseBody resp
    _    -> throwError $ show (responseStatus resp) ++ "\n" ++
                         either id showValue (eitherDecode $ responseBody resp)

httpPOST :: FromJSON a => SignedManager -> String -> [(BS.ByteString, BS.ByteString)] -> EO a
httpPOST (man, oth, cred) url prm = do
  resp <- flip httpLbs man =<< signOAuth oth cred =<< urlEncodedBody prm <$> parseRequest url
  case statusCode (responseStatus resp) of
    200  -> liftEither $ eitherDecode $ responseBody resp
    _    -> throwError $ show (responseStatus resp) ++ "\n" ++
                         either id showValue (eitherDecode $ responseBody resp)

getFollowerList :: SignedManager -> String -> EO TwUserList
getFollowerList man cur = httpGET man $
  "https://api.twitter.com/1.1/followers/list.json?count=200&cursor=" ++ cur

getFriendsList :: SignedManager -> String -> EO TwUserList
getFriendsList man cur = httpGET man $
  "https://api.twitter.com/1.1/friends/list.json?count=200&cursor=" ++ cur

getUserListAll :: (String -> EO TwUserList) -> EO [TwUser]
getUserListAll getList = concat <$> f "-1"
  where
    f "0" = return []
    f cur = getList cur >>= \list -> (users list :) <$> f (next_cursor_str list)

getUserList :: EO (Followers, Friends)
getUserList = do
  man <- mkSignedManager
  wer <- map followerToUser <$> getUserListAll (getFollowerList man)
  ing <- map (friendToUser wer) <$> getUserListAll (getFriendsList man)
  return (Followers wer, Friends ing)
    where
      followerToUser :: TwUser -> User
      followerToUser x = User
        { idStr      = id_str       x
        , screenName = screen_name  x
        , friendShip = if following x then Friend else FollowedBy
        }
      friendToUser :: [User] -> TwUser -> User
      friendToUser ws x = User
        { idStr      = id_str      x
        , screenName = screen_name x
        , friendShip = nullIf Following (const Friend) $ filter (\w -> id_str x == idStr w) ws
        }

getUserId :: String -> EO String
getUserId sname = do
  man <- mkSignedManager
  let url = "https://api.twitter.com/1.1/users/show.json?screen_name=" ++ sname
  id_str <$> httpGET man url

getScreenName :: String -> EO String
getScreenName uid = do
  man <- mkSignedManager
  let url = "https://api.twitter.com/1.1/users/show.json?user_id=" ++ uid
  screen_name <$> httpGET man url

requestTwitter :: String -> EO Value
requestTwitter url = do
  man <- mkSignedManager
  httpGET man url

tweet :: String -> EO Value
tweet tw = do
  man <- mkSignedManager
  let url = "https://api.twitter.com/1.1/statuses/update.json"
      prm = [(BS.pack "status", encodeUtf8 $ T.pack tw)]
  httpPOST man url prm

addListMember :: String -> String -> EO Value
addListMember lid uid = do
  man <- mkSignedManager
  let url = "https://api.twitter.com/1.1/lists/members/create.json"
      prm = [(BS.pack "list_id", encodeUtf8 $ T.pack lid)
            ,(BS.pack "user_id", encodeUtf8 $ T.pack uid)]
  httpPOST man url prm

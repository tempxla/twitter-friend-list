module Types
  ( EO
  , FriendShip (..)
  , User (..)
  , Followers (..)
  , Friends (..)
  ) where

import           Control.Monad.Except

type EO = ExceptT String IO

data FriendShip = Following | FollowedBy | Friend
  deriving (Show, Read, Eq)

data User = User
  { idStr      :: String
  , screenName :: String
  , friendShip :: FriendShip
  } deriving (Show, Read, Eq)

newtype Followers = Followers { followersToUsers :: [User] }
newtype Friends = Friends { friendsToUsers :: [User] }

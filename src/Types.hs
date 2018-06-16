module Types
  ( EO
  , FriendShip (..)
  , User (..)
  ) where

import           Control.Monad.Except

type EO = ExceptT String IO

data FriendShip = Following | Followered | Friend
  deriving (Show, Read, Eq)

data User = User
  { idStr      :: String
  , screenName :: String
  , friendShip :: FriendShip
  } deriving (Show, Read, Eq)

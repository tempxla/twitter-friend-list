module Types
  ( EO
  , FriendShip (..)
  , User (..)
  ) where

import           Control.Monad.Except
import qualified Data.Text            as T

type EO = ExceptT T.Text IO

data FriendShip = Following | FollowedBy | Friend
  deriving (Show, Read, Eq)

data User = User
  { idStr      :: T.Text
  , screenName :: T.Text
  , friendShip :: FriendShip
  } deriving (Show, Read, Eq)

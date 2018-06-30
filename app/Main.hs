{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text          as T
import qualified Data.Text.IO       as TO
import           Output
import           System.Environment

main :: IO ()
main = do
  createConfigDirectoryIfMissing
  args <- getArgs
  case args of
    []                    -> downloadAndDiff
    ["diff"]              -> diffLatestUserList
    ["list"]              -> listUsers 0
    ["list", n]           -> listUsers (read n)
    ["show", name]        -> getUserId $ T.pack name
    ["show", "-i", uid]   -> getScreenName $ T.pack uid
    ["debug", "GET", url] -> requestTwitter $ T.pack url
    _                     -> desc

desc :: IO ()
desc = mapM_ TO.putStrLn
  ["options:                                                                        "
  ,"  (no options)       - download follower/following lists and compare old lists. "
  ,"  diff               - compare latest list. (not download)                      "
  ,"  list n             - print a friend list. (not download)                      "
  ,"                       n is a back number. (default 0, which means lastest)     "
  ,"  show screen_name   - get user_id by screen_name.                              "
  ,"  show -i user_id    - get screen_name by user_id.                              "
  ]

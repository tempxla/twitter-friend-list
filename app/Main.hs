module Main where

import           Output
import           System.Environment

main :: IO ()
main = do
  createConfigDirectoryIfMissing
  args <- getArgs
  case args of
    []            -> downloadAndDiff
    ["diff"]      -> diffLatestUserList
    ["list"]      -> listUsers 0
    ["list", n]   -> listUsers (read n)
    _                 -> do
      putStrLn   "options:      "
      putStrLn $ "  (no options)" ++ "\t" ++ "download and diff list.           "
      putStrLn $ "  diff        " ++ "\t" ++ "diff latest list. (not download)  "
      putStrLn $ "  list [n]    " ++ "\t" ++ "print latest list. (not download) "
      putStrLn $ "              " ++ "\t" ++ "n is back number option.          "
      putStrLn $ "              " ++ "\t" ++ "(default 0, which means lastest)  "

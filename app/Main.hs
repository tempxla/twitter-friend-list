module Main where

import           Output
import           System.Environment

main :: IO ()
main = do
  createConfigDirectoryIfMissing
  args <- getArgs
  case args of
    []       -> downloadAndDiff
    ["diff"] -> diffLatestUserList
    _        -> do
      putStrLn "options: "
      putStrLn $ "  (no options)" ++ "\t\t" ++ "download and diff list."
      putStrLn $ "  diff        " ++ "\t\t" ++ "diff latest list. (not download)"

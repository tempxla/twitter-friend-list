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
    ["list"] -> listUsers 0
    ["list", '#' : n] -> listUsers (read n)
    _        -> do
      putStrLn   "options: "
      putStrLn $ "  (no options)" ++ "\t\t" ++ "download and diff list."
      putStrLn $ "  diff        " ++ "\t\t" ++ "diff latest list. (not download)"
      putStrLn $ "  list #n     " ++ "\t\t" ++ "print latest list."
      putStrLn $ "              " ++ "\t\t" ++ "#n is back number option."
      putStrLn $ "              " ++ "\t\t" ++ "(default #0 which means lastest)"

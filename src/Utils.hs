module Utils
  ( putStrStart
  , putStrErr
  , putStrDone
  , putStrLnDone
  , eitherDo
  , tablize
  ) where

import           Data.List (transpose)

putStrStart :: String -> IO ()
putStrStart s = putStr $ "* " ++ s ++ "... "

putStrErr :: String -> IO ()
putStrErr s = putStrLn "error." >> putStrLn s

putStrDone :: String -> IO ()
putStrDone = putActDone putStr

putStrLnDone :: String -> IO ()
putStrLnDone = putActDone putStrLn

putActDone :: (String -> IO ()) -> String -> IO ()
putActDone act s = putStrLn "done." >> act s

eitherDo :: Either String a -> (a -> IO ()) -> IO ()
eitherDo x act = either putStrErr act x

tablize :: [[String]] -> String
tablize = unlines . map rows . transpose . padcols . transpose
  where
    padcols cols = map (\(i, col) -> map (pad i) col) $ zip (map maxlen cols) cols
    maxlen       = maximum . map length
    pad i z      = z ++ replicate (max 0 $ i - length z) ' '
    rows []     = ""
    rows (w:ws) = w ++ foldr (\z acc -> "    " ++ z ++ acc) "" ws

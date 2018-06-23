module Utils
  ( putStrStart
  , putStrErr
  , putStrDone
  , eitherDo
  , tablize
  , (!!?)
  , nullIf
  ) where

import           Data.List (transpose)

putStrStart :: String -> IO ()
putStrStart s = putStr $ "* " ++ s ++ "... "

putStrErr :: String -> IO ()
putStrErr s = putStrLn "error." >> putStrLn s

putStrDone :: String -> IO ()
putStrDone s = putStrLn "done." >> putStrLn s

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

infixl 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) []     _ = Nothing
(!!?) (x:_)  0 = Just x
(!!?) (_:xs) i = xs !!? (i - 1)

nullIf :: b -> ([a] -> b) -> [a] -> b
nullIf b _ [] = b
nullIf _ f xs = f xs

module Utils
  ( putStrStart
  , putStrErr
  , putStrDone
  , eitherDo
  , tablize
  , (!!?)
  , nullIf
  , showValue
  ) where

import           Data.Aeson
import qualified Data.HashMap.Strict as M
import           Data.List           (transpose)
import qualified Data.Text           as T
import qualified Data.Vector         as V

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

showValue :: Value -> String
showValue v = case v of
  (Object o) -> objf 0 o
  (Array o)  -> arrf 0 o
  o          -> sf o
  where
    obj n (t, Object o) acc = nest n ++ T.unpack t ++ " : " ++ objf n o ++ acc
    obj n (t, Array o)  acc = nest n ++ T.unpack t ++ " : " ++ arrf n o ++ acc
    obj n (t, o)        acc = nest n ++ T.unpack t ++ " : " ++ sf o ++ "\n" ++ acc
    arr n (Object o)    acc = nest n ++ objf n o ++ acc
    arr n (Array o)     acc = nest n ++ arrf n o ++ acc
    arr n o             acc = nest n ++ sf o ++ "\n" ++ acc
    objf n            = paren n '{' '}' obj . M.toList
    arrf n            = paren n '[' ']' arr . V.toList
    nest n            = replicate (n * 2) ' '
    paren _ p q _ [] = p : q : "\n"
    paren n p q f ls = p : '\n': foldr (f $ n + 1) "" ls ++ nest n ++ q : "\n"
    sf (String o) = "String \"" ++ T.unpack o ++ "\""
    sf o          = show o

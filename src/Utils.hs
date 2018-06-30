{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( putStrStart
  , putStrErr
  , putStrDone
  , eitherDo
  , tablize
  , (!!?)
  , nullIf
  , showValue
  , (＋)
  , mapLeft
  , tshow
  ) where

import           Data.Aeson
import qualified Data.HashMap.Strict as M
import           Data.List           (transpose)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TO
import qualified Data.Vector         as V

(＋) :: T.Text -> T.Text -> T.Text
(＋) = T.append

putStrStart :: T.Text -> IO ()
putStrStart s = TO.putStr $ "* " ＋ s ＋ "... "

putStrErr :: T.Text -> IO ()
putStrErr s = TO.putStrLn "error." >> TO.putStrLn s

putStrDone :: T.Text -> IO ()
putStrDone s = TO.putStrLn "done." >> TO.putStrLn s

eitherDo :: Either T.Text a -> (a -> IO ()) -> IO ()
eitherDo x act = either putStrErr act x

tablize :: [[T.Text]] -> T.Text
tablize = T.unlines . map rows . transpose . padcols . transpose
  where
    padcols cols = map (\(i, col) -> map (pad i) col) $ zip (map maxlen cols) cols
    maxlen       = maximum . map T.length
    pad i z      = z ＋ T.replicate (max 0 $ i - T.length z) " "
    rows []     = ""
    rows (w:ws) = w ＋ foldr (\z acc -> "    " ＋ z ＋ acc) "" ws

infixl 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) []     _ = Nothing
(!!?) (x:_)  0 = Just x
(!!?) (_:xs) i = xs !!? (i - 1)

nullIf :: b -> ([a] -> b) -> [a] -> b
nullIf b _ [] = b
nullIf _ f xs = f xs

showValue :: Value -> T.Text
showValue v = case v of
  (Object o) -> objf 0 o
  (Array o)  -> arrf 0 o
  o          -> sf o
  where
    obj n (t, Object o) acc = nest n ＋ t ＋ " : " ＋ objf n o ＋ acc
    obj n (t, Array o)  acc = nest n ＋ t ＋ " : " ＋ arrf n o ＋ acc
    obj n (t, o)        acc = nest n ＋ t ＋ " : " ＋ sf o ＋ "\n" ＋ acc
    arr n (Object o)    acc = nest n ＋ objf n o ＋ acc
    arr n (Array o)     acc = nest n ＋ arrf n o ＋ acc
    arr n o             acc = nest n ＋ sf o ＋ "\n" ＋ acc
    objf n            = paren n "{" "}" obj . M.toList
    arrf n            = paren n "[" "]" arr . V.toList
    nest n            = T.replicate (n * 2) " "
    paren _ p q _ [] = p ＋ q ＋ "\n"
    paren n p q f ls = p ＋ "\n" ＋ foldr (f $ n + 1) "" ls ＋ nest n ＋ q ＋ "\n"
    sf (String o) = "String \"" ＋ o ＋ "\""
    sf o          = tshow o

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft f (Left l)  = Left . f $ l
mapLeft _ (Right r) = Right r

tshow :: Show a => a -> T.Text
tshow = T.pack . show

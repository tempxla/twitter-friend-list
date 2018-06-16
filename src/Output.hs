module Output
  where

import           Control.Monad.Except
import           Data.List             (sortBy, union)
import           Data.Time.Format
import           Data.Time.LocalTime
import           System.Directory
import           System.FilePath.Posix
import qualified Twitter               as TW
import           Types

data UserDiff = Add User | Mod User User | Del User
  deriving (Show)

showUserDiff :: UserDiff -> String
showUserDiff diff = case diff of
  (Add x)   -> tab ["ADD", screenName x,       show (friendShip x),         link x]
  (Mod x y) -> tab ["MOD", sif screenName x y, sif (show . friendShip) x y, link y]
  (Del x)   -> tab ["DEL", screenName x,       show (friendShip x),         link x]
  where
    link x = "https://twitter.com/" ++ screenName x
    sif f x y = f x ++ if f x == f y then "" else "->" ++ f y
    tab []     =  ""
    tab (w:ws) = w ++ go ws
      where
        go []     = ""
        go (v:vs) = '\t' : (v ++ go vs)

configDir :: String
configDir = ".twitter-friend-list"

getConfigPath :: IO String
getConfigPath = fmap (</> configDir) getHomeDirectory

getCurrentDateTime :: IO String
getCurrentDateTime = formatTime defaultTimeLocale "%y%m%d%H%M%S" <$> getZonedTime

outputUserList :: String -> [User] -> [User] -> IO String
outputUserList confDir wer ing = do
  outDir <- (confDir </>) <$> getCurrentDateTime
  createDirectory outDir
  writeFile (outDir </> "follower.txt") $ unlines $ map show wer
  writeFile (outDir </> "folloing.txt") $ unlines $ map show ing
  return outDir

downloadUserList :: IO ()
downloadUserList = do
  putStr "download... "
  confDir <- getConfigPath
  createDirectoryIfMissing False confDir
  userList <- runExceptT TW.getUserList
  case userList of
    Left e           -> putStrLn "error.:" >> putStrLn e
    Right (wer, ing) -> do
      p <- outputUserList confDir wer ing
      putStrLn "done.:"
      putStrLn p

loadLatestUserList :: IO (Either String ([User], [User]))
loadLatestUserList = do
  confDir <- getConfigPath
  dirs <- take 2 . sortBy (flip compare) <$> listDirectory confDir
  case dirs of
    []         -> return $ Left "user list not found."
    [new]      -> return $ Left "it can't run diff since found only one user list."
    [new, old] -> do
      wer  <- readFunc $ confDir </> old </> "follower.txt"
      ing  <- readFunc $ confDir </> old </> "folloing.txt"
      wer' <- readFunc $ confDir </> new </> "follower.txt"
      ing' <- readFunc $ confDir </> new </> "folloing.txt"
      return $ Right (wer `union` ing, wer' `union` ing')
  where
    readFunc path = map read . lines <$> readFile path

makeDiffUserList :: [User] -> [User] -> [UserDiff]
makeDiffUserList old new = older old new ++ newer old new
  where
    older xs ys = foldr (\x acc -> maybe acc (:acc) $ f x ys) [] xs
    f x (y:ys)
      | x == y             = Nothing
      | idStr x == idStr y = Just $ Mod x y
      | otherwise          = f x ys
    f x []                 = Just $ Del x
    newer xs = map Add . filter (flip notElem (map idStr xs) . idStr)

diffLatestUserList :: IO ()
diffLatestUserList = do
  putStr "diff lastest list... "
  list <- loadLatestUserList
  case list of
    Left e -> putStrLn "error.:" >> putStrLn e
    Right (old, new) -> do
      putStrLn "done.:"
      mapM_ (putStrLn . showUserDiff) $ makeDiffUserList old new

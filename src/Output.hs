module Output
  where

import           Control.Monad.Except
import           Data.Time.Format
import           Data.Time.LocalTime
import           System.Directory
import           System.FilePath.Posix
import qualified Twitter               as TW
import           Types

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
  putStrLn "download..."
  confDir <- getConfigPath
  createDirectoryIfMissing False confDir
  userList <- runExceptT TW.getUserList
  case userList of
    Left e           -> putStrLn $ "error.: " ++ e
    Right (wer, ing) -> do
      p <- outputUserList confDir wer ing
      putStrLn $ "done.: " ++ p

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

outputUserList :: String -> [User] -> [User] -> IO ()
outputUserList confDir wer ing = do
  dir <- getCurrentDateTime
  let outDir = confDir </> dir
  createDirectory outDir
  writeFile (outDir </> "follower.txt") $ unlines $ map show wer
  writeFile (outDir </> "folloing.txt") $ unlines $ map show ing

downloadUserList :: IO ()
downloadUserList = do
  confDir <- getConfigPath
  createDirectoryIfMissing False confDir
  userList <- runExceptT TW.getUserList
  case userList of
    Left e           -> print e
    Right (wer, ing) -> outputUserList confDir wer ing

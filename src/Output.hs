module Output
  ( createConfigDirectoryIfMissing
  , diffLatestUserList
  , downloadAndDiff
  , listUsers
  , getUserId
  , getScreenName
  , requestTwitter
  , tweet
  ) where

import           Control.Monad.Except
import           Data.List             (sortBy, union)
import           Data.Time.Format
import           Data.Time.LocalTime
import           System.Directory
import           System.FilePath.Posix
import qualified Twitter               as TW
import           Types
import           Utils

data UserDiff = Add User | Mod User User | Del User
  deriving (Show)

showUserDiff :: UserDiff -> [String]
showUserDiff diff = case diff of
  (Add x)   -> ["ADD", screenName x,       show (friendShip x),         link x]
  (Mod x y) -> ["MOD", sif screenName x y, sif (show . friendShip) x y, link y]
  (Del x)   -> ["DEL", screenName x,       show (friendShip x),         link x]
  where
    link x = "https://twitter.com/" ++ screenName x
    sif f x y = f x ++ if f x == f y then "" else "->" ++ f y

showUser :: User -> [String]
showUser u = [idStr u, screenName u, show (friendShip u), link u]
  where
    link x = "https://twitter.com/" ++ screenName x

configDir :: String
configDir = ".twitter-friend-list"

getConfigPath :: IO String
getConfigPath = fmap (</> configDir) getHomeDirectory

getCurrentDateTime :: IO String
getCurrentDateTime = formatTime defaultTimeLocale "%y%m%d%H%M%S" <$> getZonedTime

outputUserList :: String -> Followers -> Friends -> IO String
outputUserList confDir wer ing = do
  outDir <- (confDir </>) <$> getCurrentDateTime
  createDirectory outDir
  writeFile (outDir </> "followers.txt") . unlines . map show $ followersToUsers wer
  writeFile (outDir </> "following.txt") . unlines . map show $ friendsToUsers ing
  return outDir

takeUserList :: Int -> IO [[User]]
takeUserList i = do
  putStrStart "load"
  confDir <- getConfigPath
  list <- take i . sortBy (flip compare) <$> listDirectory confDir
  forM list $ \dir -> do
    let r file = map read . lines <$> readFile (confDir </> dir </> file)
    union <$> r "followers.txt" <*> r "following.txt"

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

createConfigDirectoryIfMissing :: IO ()
createConfigDirectoryIfMissing = createDirectoryIfMissing False =<< getConfigPath

downloadUserList :: EO [User]
downloadUserList = do
  liftIO $ putStrStart "download"
  (wer, ing) <- TW.getUserList
  liftIO $ do
    confDir <- getConfigPath
    putStrDone =<< outputUserList confDir wer ing
  return $ union (followersToUsers wer) (friendsToUsers ing)

downloadAndDiff :: IO ()
downloadAndDiff = do
  list <- takeUserList 1
  newE <- runExceptT downloadUserList
  eitherDo newE $ \new -> case list of
    []    -> putStrDone =<< diffUserList [] new
    [old] -> putStrDone =<< diffUserList old new
    _     -> putStrErr "load"

diffLatestUserList :: IO ()
diffLatestUserList = do
  list <- takeUserList 2
  case list of
    []         -> putStrErr "user list not found."
    [new]      -> putStrDone =<< diffUserList [] new
    [new, old] -> putStrDone =<< diffUserList old new
    _          -> putStrErr "load"

diffUserList :: [User] -> [User] -> IO String
diffUserList old new = do
  putStrStart "diff"
  let result = if   null old
               then tablize $ map (showUserDiff . Add) new
               else tablize $ map showUserDiff $ makeDiffUserList old new
  return $ if result == "" then "no changes." else result

listUsers :: Int -> IO ()
listUsers i = do
  list <- (!!? i) <$> takeUserList (i + 1)
  case list of
    Nothing    -> putStrErr "user list not found."
    Just users -> putStrDone $ tablize $ map showUser users

getUserId :: String -> IO ()
getUserId sname = do
  putStrStart "get user id"
  e <- runExceptT $ TW.getUserId sname
  eitherDo e $ \uid -> putStrDone $ tablize [[uid, sname, "https://twitter.com/" ++ sname]]

getScreenName :: String -> IO ()
getScreenName uid = do
  putStrStart "get screen name"
  e <- runExceptT $ TW.getScreenName uid
  eitherDo e $ \sname -> putStrDone $ tablize [[uid, sname, "https://twitter.com/" ++ sname]]

requestTwitter :: String -> IO ()
requestTwitter url = do
  putStrStart "GET"
  e <- runExceptT $ TW.requestTwitter url
  eitherDo e $ \v -> putStrDone $ showValue v

tweet :: String -> IO ()
tweet tw = do
  putStrStart "tweet"
  e <- runExceptT $ TW.tweet tw
  eitherDo e $ \_ -> putStrDone ""

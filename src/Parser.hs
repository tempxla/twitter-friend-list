module Parser (readTwitApiKeys) where

import           System.Directory
import           System.FilePath.Posix
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import           Text.Parsec.String

data TwitApiKeys = TwitApiKeys {
  consumerKey         :: String
  , consumerSecret    :: String
  , accessToken       :: String
  , accessTokenSecret :: String
  } deriving (Show)

readTwitApiKeys :: IO (Either String TwitApiKeys)
readTwitApiKeys = do
  filePath <- fmap (</> ".twitter_api_keys") getHomeDirectory
  either (Left . show) mkTwitApiKeys <$> parseFromFile parseKeyValueList filePath

mkTwitApiKeys :: [(String, String)] -> Either String TwitApiKeys
mkTwitApiKeys kvs = do
  let lookupE key = maybe (Left $ "not found.: " ++ key) Right $ lookup key kvs
  ck <- lookupE "consumer_key"
  cs <- lookupE "consumer_secret"
  at <- lookupE "access_token"
  as <- lookupE "access_token_secret"
  return $ TwitApiKeys { consumerKey       = ck
                       , consumerSecret    = cs
                       , accessToken       = at
                       , accessTokenSecret = as
                       }

parseKeyValueList :: Parser [(String, String)]
parseKeyValueList = parseKeyValue `sepEndBy` endOfLine

parseKeyValue :: Parser (String, String)
parseKeyValue = do
  key <- sp *> many1 (alphaNum <|> char '_') <* sp
  _ <- char '='
  value <- sp *> char '"' *> many1 (noneOf "\"") <* char '"' <* sp
  return (key, value)

sp :: Parser String
sp = many $ oneOf " \t"

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Database.SQLite.Simple
import GHC.Generics
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client

-- | This datatype represents a Post Message that the /words endpoint accepts
newtype Message = Message {word :: String}
  deriving (Generic, Show)

instance FromJSON Message

instance ToJSON Message

-- | This datatype represents the types of words we store in our database
newtype WordType = WordType
  { wordTypeName :: Text
  }
  deriving (Show, Generic)

adjective :: WordType
adjective = WordType "adjective"

noun :: WordType
noun = WordType "noun"

instance FromHttpApiData WordType where
  parseUrlPiece wtype
    | wtype == wordTypeName adjective = Right adjective
    | wtype == wordTypeName noun = Right noun
    | otherwise = Left wtype

instance ToHttpApiData WordType where
  toUrlPiece = wordTypeName

data AllWords = AllWords {nouns :: [String], adjectives :: [String]}
  deriving (Generic, Show)

instance FromJSON AllWords

instance ToJSON AllWords

-- | Our API type
type API =
  -- | a GET to / returns all the words in the db
  Get '[JSON] AllWords
    -- \| a GET to /random returns a random insult
    :<|> "random" :> Get '[JSON] String
    -- \| a POST to /words/<noun | adjective> will add a new word to the db
    :<|> "words" :> Capture "wordtype" WordType :> ReqBody '[JSON] Message :> Post '[JSON] NoContent
    -- \| a DELETE to /words/<noun | adjective>?word=<word> will delete the word from the db
    :<|> "words" :> Capture "wordtype" WordType :> Capture "word" String :> Delete '[JSON] NoContent

api :: Proxy API
api = Proxy

initDB :: FilePath -> IO ()
initDB dbfile = withConnection dbfile $ \conn -> do
  -- Create
  execute_ conn "CREATE TABLE IF NOT EXISTS noun (word TEXT NOT NULl UNIQUE)"
  execute_ conn "CREATE TABLE IF NOT EXISTS adjective (word TEXT NOT NULl UNIQUE)"
  -- Seed the db with our signature insults
  let seedData = [(adjective, "snivelling"), (noun, "servant"), (adjective, "bumbling"), (noun, "trumpet")]

  mapM_ (\(t, w) -> insertWord t w conn) seedData

port :: Port
port = 8081

insertWord :: WordType -> String -> Connection -> IO ()
insertWord wtype word conn =
  execute
    conn
    (Query $ "INSERT OR IGNORE INTO " <> wordTypeName wtype <> " (word) VALUES (?)")
    (Only word)

deleteWord :: WordType -> String -> Connection -> IO ()
deleteWord wtype word conn =
  execute
    conn
    (Query $ "DELETE FROM " <> wordTypeName wtype <> " WHERE word = (?)")
    (Only word)

selectAllWords :: WordType -> Connection -> IO [String]
selectAllWords wtype conn =
  map fromOnly <$> query_ conn (Query ("SELECT word FROM " <> wordTypeName wtype))

server :: FilePath -> Server API
server dbfile = getAll :<|> getRandom :<|> postWord :<|> delWord
  where
    postWord :: WordType -> Message -> Handler NoContent
    postWord wordtype msg = do
      liftIO . withConnection dbfile $ insertWord wordtype (word msg)
      return NoContent

    delWord :: WordType -> String -> Handler NoContent
    delWord wordtype word = do
      liftIO . withConnection dbfile $ deleteWord wordtype word
      return NoContent

    getAll :: Handler AllWords
    getAll = liftA2 AllWords (getWords noun) (getWords adjective)
      where
        getWords wtype = liftIO . withConnection dbfile $ selectAllWords wtype

    getRandom :: Handler String
    getRandom =
      fmap (head . map fromOnly) . liftIO $
        withConnection dbfile $
          \conn -> query_ conn "SELECT (SELECT word FROM adjective ORDER BY RANDOM() LIMIT 1) || ' ' || (SELECT word FROM noun ORDER BY RANDOM() LIMIT 1);"

runApp :: FilePath -> IO ()
runApp dbfile = do
  putStrLn $ "Server running on port " ++ show port
  run port (serve api $ server dbfile)

postWord :: WordType -> Message -> ClientM NoContent
delWord :: WordType -> String -> ClientM NoContent
getRnd :: ClientM String
getAll :: ClientM AllWords
getAll :<|> getRnd :<|> postWord :<|> delWord = client api

main :: IO ()
main = do
  let dbfile = "word.db"
  initDB dbfile
  runApp dbfile

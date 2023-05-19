{-# LANGUAGE OverloadedStrings #-}
module Server(runServer) where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run, Port)
import Person (jsonPersons, readPerson, jsonPerson)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (fromStrict)
import DBfuncs (getAllPersons, insertPerson, dbInit, getPerson)
import qualified Data.Text as T

port :: Port
port = 8080

runServer :: IO ()
runServer = run port app

app :: Application
app request response = do
  dbInit
  case pathInfo request of
    ["people"]   -> allPeople request response
    ["people",i] -> showPerson (read . T.unpack $ i) response
    _            -> showEmpty response

getAll :: (Response -> IO b) -> IO b
getAll response = do
      people <- getAllPersons
      response . responseLBS status200 []. fromStrict . encodeUtf8 . jsonPersons $ people

postPerson :: Application
postPerson request response = do
      body <- strictRequestBody request
      case readPerson body of
        (Left msg) -> response . responseLBS status401 [] $ msg
        (Right p)  -> do
          insertPerson p
          getAll response


allPeople :: Application
allPeople request response =
  case requestMethod request of
    "GET"  -> getAll response
    "POST" -> postPerson request response
    _      -> showEmpty response

showPerson :: Int -> (Response -> IO b) -> IO b
showPerson i response = do
  [person] <- getPerson i
  response . responseLBS status200 [] .fromStrict . encodeUtf8 . jsonPerson $ person

showEmpty :: (Response -> IO b) -> IO b
showEmpty response = response $ responseLBS status404 [] "404: Could not find this item"

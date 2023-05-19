{-# LANGUAGE OverloadedStrings #-}

module DBfuncs(insertPerson, getAllPersons, getPerson, dbInit) where

import Database.SQLite.Simple
import Person

dbConnection :: IO Connection
dbConnection = open "People.db"

dbInit :: IO ()
dbInit = do
  conn <- dbConnection
  execute_ conn "CREATE TABLE IF NOT EXISTS People \
                             \(Id INTEGER NOT NULL PRIMARY KEY,\
                             \ FName VARCHAR(30) NOT NULL,\
                             \ LName VARCHAR(30) NOT NULL)"

getAllPersons :: IO [Person]
getAllPersons = do
  conn <- dbConnection 
  query_ conn "SELECT Id, FName, LName FROM People"

getPerson ::  Int -> IO [Person]
getPerson i = do
  conn <- dbConnection
  query conn "SELECT Id, FName, LName FROM People WHERE Id == ?" (Only i)

insertPerson :: Person -> IO ()
insertPerson (Person _ fn ln) = do
  conn <- dbConnection
  execute conn "INSERT INTO People (FName, LName) VALUES (?,?)" (fn,ln)
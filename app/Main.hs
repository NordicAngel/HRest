module Main (main) where

import Server

main :: IO ()
main = do
  putStrLn "Server is runnig" 
  runServer

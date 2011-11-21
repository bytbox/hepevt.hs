module Main where

import Data.HEPEVT

main = do
  events <- parseEventFile "events.dat"
  putStrLn $ show events


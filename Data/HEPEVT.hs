{-|

This modules provides a pure haskell implementation of a reader and writer for
the ASCII HEPEVT file format.

-}
module Data.HEPEVT (
  Event,

  parseEventFile,
  parseEvents,
) where

type Event = ([Double], [[Double]])

parseEventFile :: String -> IO [Event]
parseEventFile fname = do
  readFile fname >>= return . parseEvents fname

parseEvents :: String -> String -> [Event]
parseEvents fname dat =
  let ls = lines dat in
    []

{-|

This modules provides a pure haskell implementation of a reader and writer for
the ASCII HEPEVT file format.

-}
module Data.HEPEVT (
  Event,

  parseEventFile,
  parseEvents,
) where

import qualified Data.ByteString.Char8 as S

type Event = ([Double], [[Double]])

parseEventFile :: String -> IO [Event]
parseEventFile fname = do
  S.readFile fname >>= return . parseEvents fname

parseEvents :: String -> S.ByteString -> [Event]
parseEvents fname dat =
  let ls = S.split '\n' dat in 
    foldl procLine [] ls
  where
    procLine events line =
      let ws = S.split ' ' line in
        if length ws < 1 then events else
          let h = head ws in
            process (S.unpack h) events (tail ws)
    process "P" events line = ([], []) : events
    process _ events _ = events

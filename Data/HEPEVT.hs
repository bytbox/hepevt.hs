{-|

This module provides a pure haskell implementation of a reader and writer for
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
  S.readFile fname >>= return . parseEvents

data Line = Meta String | E [Double] | P [Double] | V [Double] | Blank
  deriving (Show, Eq)

parseLine :: S.ByteString -> Line
parseLine line =
  let ws = S.words line in
    parseLine' (map S.unpack ws) line
  where
    parseLine' ("E":xs) _ = E $ map parseDouble xs
    parseLine' ("P":xs) _ = P $ map parseDouble xs
    parseLine' ("V":xs) _ = V $ map parseDouble xs
    parseLine' _ line = if S.length line > 1 then Meta (S.unpack line) else Blank

getLines :: [S.ByteString] -> [Line]
getLines = map parseLine

type ParseState = [Event]

parseEvents :: S.ByteString -> [Event]
parseEvents dat =
  let ls = S.split '\n' dat in 
    reverse $ foldl process [] $ getLines ls
  where
    process events (E ds) = (ds, []):events
    process ((el, vl):events) (V ds) = (el, ds:vl):events
    process events _ = events

parseDouble :: String -> Double
parseDouble = read . reverse . dropWhile (=='.') . reverse

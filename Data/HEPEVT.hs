{-|

This module provides a pure haskell implementation of a reader and writer for
the ASCII HEPEVT file format, rougly as described at
http://cepa.fnal.gov/psm/simulation/mcgen/lund/pythia_manual/pythia6.3/pythia6301/node39.html.
Compatibility with the extended format used by HepMC
(http://lcgapp.cern.ch/project/simu/HepMC/) is given priority.

-}
module Data.HEPEVT (
  Event, Particle(..),

  parseEventFile,
  parseEvents,
) where

import qualified Data.ByteString.Char8 as S

{-
int: barcode
int: PDG id
double: px
double: py
double: pz
double: energy
double: generated mass
int: status code
double: Polarization theta
double: Polarization phi
int: barcode for vertex that has this particle as an incoming particle
int: number of entries in flow list (may be zero)
int, int: optional code index and code for each entry in the flow list
-}
data Particle = Particle
  { barcode :: Int
  , pid     :: Int
  , px      :: Double
  , py      :: Double
  , pz      :: Double
  , energy  :: Double
  , mass    :: Double
  , stat    :: Int
  , ptheta  :: Double
  , pphi    :: Double
  }
  deriving (Eq, Show)

mkParticle :: [String] -> Particle
mkParticle (barcode:pid:px:py:pz:energy:mass:stat:ptheta:pphi:_) =
  Particle
    { barcode = read barcode
    , pid     = read pid
    , px      = parseDouble px
    , py      = parseDouble py
    , pz      = parseDouble pz
    , energy  = parseDouble energy
    , mass    = parseDouble mass
    , stat    = read stat
    , ptheta  = parseDouble ptheta
    , pphi    = parseDouble pphi
    }

type Event = ([Double], [Particle])

parseEventFile :: String -> IO [Event]
parseEventFile fname = do
  S.readFile fname >>= return . parseEvents

data Line = Meta String | E [Double] | P Particle | V [Double] | Blank
  deriving (Show, Eq)

parseLine :: S.ByteString -> Line
parseLine line =
  let ws = S.words line in
    parseLine' (map S.unpack ws) line
  where
    parseLine' ("E":xs) _ = E $ map parseDouble xs
    parseLine' ("P":xs) _ = P $ mkParticle xs
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
    process ((el, pl):events) (P p) = (el, p:pl):events
    process events _ = events

parseDouble :: String -> Double
parseDouble = read . reverse . dropWhile (=='.') . reverse

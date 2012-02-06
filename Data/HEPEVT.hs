{-|

This module provides a pure haskell implementation of a reader for the ASCII
HEPEVT file format, rougly as described at
<http://cepa.fnal.gov/psm/simulation/mcgen/lund/pythia_manual/pythia6.3/pythia6301/node39.html>.
Compatibility with the extended format used by HepMC
(<http://lcgapp.cern.ch/project/simu/HepMC/>) is given priority.

-}
module Data.HEPEVT (
  parseEventFile,
  parseEvents,
) where

import qualified Data.ByteString.Char8 as S

import Data.LHA

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

mkParticle :: [String] -> Particle
mkParticle (barcode:pid:px:py:pz:energy:mass:stat:ptheta:pphi:_) =
  Particle
    { partPDG   = read pid
    , partPx    = parseDouble px
    , partPy    = parseDouble py
    , partPz    = parseDouble pz
    , partE     = parseDouble energy
    , partM     = parseDouble mass
    , status    = statusFromInt $ read stat
    , mothers   = PZero
    , iColor    = (0, 0)
    , lifetime  = 0
    , spin      = 0
    }

parseEventFile :: String -> IO [Event]
parseEventFile fname = do
  S.readFile fname >>= return . parseEvents

data Line = Meta String | E [Double] | P Particle | V [Double] | Blank

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

eventOf :: ([Double], [Particle]) -> Event
eventOf ((_:_:scale:aQCD:aQED:_), ps) = Event
  { nPart = length ps
  , evProcId = 0
  , evWeight = 0
  , scale = scale
  , aQCD = aQCD
  , aQED = aQED
  , parts = ps
  }

parseEvents :: S.ByteString -> [Event]
parseEvents dat = map eventOf $
  let ls = S.split '\n' dat in 
    reverse $ foldl process [] $ getLines ls
  where
    process events (E ds) = (ds, []):events
    process ((el, pl):events) (P p) = (el,p:pl):events
    process events _ = events

parseDouble :: String -> Double
parseDouble = read . reverse . dropWhile (=='.') . reverse

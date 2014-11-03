module Data.HepMC.EventInfo where

import Data.HepMC.Parser.Common

data EventInfo = EventInfo {
    eventNumber :: Int,
    nMultPartInts :: Int,
    eventScale :: Double,
    alphaQCD :: Double,
    alphaQED :: Double,
    signalProcessID :: Int,
    signalProcessBarcode :: Int,
    nVertices :: Int,
    beamParticleBarcodes :: (Int, Int),
    nRndmStateInts :: Int,
    rndmStateInts :: [Int],
    nEventWeights :: Int,
    eventWeights :: [Double]
} deriving (Eq, Ord, Read, Show)


parseEventInfo :: Parser EventInfo
parseEventInfo = do
    char 'E'; skipSpace
    en <- dec
    nmpi <- dec
    scale <- doub
    aqcd <- doub
    aqed <- doub
    spid <- dec
    spbc <- dec
    nvtx <- dec
    bc1 <- dec
    bc2 <- dec
    let bpbcs = (bc1, bc2)

    nrs <- dec
    rs <- parseList nrs (signed decimal)

    nevtwgts <- dec
    evtwgts <- parseList nevtwgts $ (signed double)

    skipSpace

    return $
        EventInfo en nmpi scale aqcd aqed spid spbc nvtx bpbcs nrs rs nevtwgts evtwgts

module Data.HepMC.EventInfo where

import Data.HepMC.Parse

data EventInfo = EventInfo {
    eventNumber :: Int,
    nMultPartInts :: Int,
    eventScale :: Double,
    alphaQCD :: Double,
    alphaQED :: Double,
    signalProcessID :: Int,
    signalProcessBC :: Int,
    nVertices :: Int,
    beamParticleBCs :: (Int, Int),
    nRndmStateInts :: Int,
    rndmStateInts :: [Int],
    nEventWeights :: Int,
    eventWeights :: [Double]
} deriving (Eq, Ord, Read, Show)


parseEventInfo :: Parser EventInfo
parseEventInfo = do
    _ <- char 'E'<* skipSpace
    en <- decSpace
    nmpi <- decSpace
    scale <- doubSpace
    aqcd <- doubSpace
    aqed <- doubSpace
    spid <- decSpace
    spbc <- decSpace
    nvtx <- decSpace
    bpbcs <- tuple decSpace decSpace

    nrs <- decSpace
    rs <- count nrs decSpace

    nevtwgts <- decSpace
    evtwgts <- count nevtwgts doubSpace

    return $
        EventInfo en nmpi scale aqcd aqed spid spbc nvtx bpbcs nrs rs nevtwgts evtwgts

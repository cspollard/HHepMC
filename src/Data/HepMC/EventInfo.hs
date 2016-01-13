module Data.HepMC.EventInfo where

import Data.HepMC.Parse
import Data.Vector

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
    rndmStateInts :: Vector Int,
    eventWeights :: Vector Double
} deriving (Eq, Ord, Read, Show)


parserEventInfo :: Parser EventInfo
parserEventInfo = do
    char 'E' >> skipSpace
    EventInfo
        <$> decimal <* skipSpace
        <*> decimal <* skipSpace
        <*> double <* skipSpace
        <*> double <* skipSpace
        <*> double <* skipSpace
        <*> decimal <* skipSpace
        <*> decimal <* skipSpace
        <*> decimal <* skipSpace
        <*> tuple (decimal <* skipSpace) (decimal <* skipSpace)
        <*> hepmcList decimal
        <*> hepmcList double

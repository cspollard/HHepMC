module Data.HepMC.HeavyIonInfo where

data HeavyIonInfo = HeavyIonInfo {
    nHardScatters :: Int,
    nProjParts :: Int,
    nTargParts :: Int,
    nNNInts :: Int,
    nSpectNeuts :: Int,
    nSpectProts :: Int,
    nNNwoundColls :: Int,
    nNwoundNColls :: Int,
    nNwoundNwoundColls :: Int,
    collImpactParam :: Double,
    eventPlaneAzimuth :: Double,
    nucleonEccent :: Double,
    inelastCrossSec :: Double
} deriving (Eq, Ord, Read, Show)

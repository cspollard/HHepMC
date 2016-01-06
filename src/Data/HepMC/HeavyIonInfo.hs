module Data.HepMC.HeavyIonInfo where

import Data.HepMC.Parse

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


parserHeavyIonInfo :: Parser HeavyIonInfo
parserHeavyIonInfo = do
    char 'H' >> skipSpace
    HeavyIonInfo
        <$> decimal <* skipSpace
        <*> decimal <* skipSpace
        <*> decimal <* skipSpace
        <*> decimal <* skipSpace
        <*> decimal <* skipSpace
        <*> decimal <* skipSpace
        <*> decimal <* skipSpace
        <*> decimal <* skipSpace
        <*> decimal <* skipSpace
        <*> double <* skipSpace
        <*> double <* skipSpace
        <*> double <* skipSpace
        <*> double <* skipSpace

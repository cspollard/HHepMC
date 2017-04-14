{-# LANGUAGE TemplateHaskell #-}

module HepMC.HeavyIonInfo where

import           Control.Lens
import           HepMC.Parse

data HeavyIonInfo =
  HeavyIonInfo
    { _nHardScatters      :: Int
    , _nProjParts         :: Int
    , _nTargParts         :: Int
    , _nNNInts            :: Int
    , _nSpectNeuts        :: Int
    , _nSpectProts        :: Int
    , _nNNwoundColls      :: Int
    , _nNwoundNColls      :: Int
    , _nNwoundNwoundColls :: Int
    , _collImpactParam    :: Double
    , _eventPlaneAzimuth  :: Double
    , _nucleonEccent      :: Double
    , _inelastCrossSec    :: Double
    } deriving Show

makeLenses ''HeavyIonInfo


parserHeavyIonInfo :: Parser HeavyIonInfo
parserHeavyIonInfo = do
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

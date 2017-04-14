{-# LANGUAGE TemplateHaskell #-}

module HepMC.EventInfo where

import Control.Lens

import HepMC.Parse

data EventInfo =
    EventInfo
        { _eventNumber :: Int
        , _nMultPartInts :: Int
        , _eventScale :: Double
        , _alphaQCD :: Double
        , _alphaQED :: Double
        , _signalProcessID :: Int
        , _signalProcessBC :: Int
        , _nVertices :: Int
        , _beamParticleBCs :: (Int, Int)
        , _rndmStateInts :: [Int]
        , _eventWeights :: [Double]
        } deriving (Eq, Ord, Show)

makeLenses ''EventInfo


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

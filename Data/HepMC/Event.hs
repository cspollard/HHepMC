module Data.HepMC.Event where

import qualified Data.Text.Lazy as TL
import Data.HepMC.Vertex
import Data.HepMC.Particle

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
} deriving (Read, Show)


type WeightNames = [TL.Text]


data UnitEnergy = MEV | GEV deriving (Eq, Ord, Read, Show)
data UnitLength = MM | CM deriving (Eq, Ord, Read, Show)


data Units = Units {
    unitEnergy :: UnitEnergy,
    unitLength :: UnitLength
} deriving (Eq, Ord, Read, Show)


type CrossSection = (Double, Double)


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


data PDFInfo = PDFInfo {
    pdfID1 :: Int,
    pdfID2 :: Int,
    pdfX1 :: Double,
    pdfX2 :: Double,
    qScale :: Double,
    pdfXfx1 :: Double,
    pdfXfx2 :: Double,
    pdfSetID1 :: Int,
    pdfSetID2 :: Int
} deriving (Eq, Ord, Read, Show)


data EventHeader = EventHeader {
    eventInfo :: EventInfo,
    weightNames :: Maybe WeightNames,
    units :: Units,
    crossSection :: Maybe CrossSection,
    heavyIonInfo :: Maybe HeavyIonInfo,
    pdfInfo :: Maybe PDFInfo
} deriving (Eq, Ord, Read, Show)


data Event = Event {
    eventHeader :: EventHeader,
    eventVertices :: Vertices,
    eventParticles :: Particles
} deriving (Eq, Ord, Read, Show)

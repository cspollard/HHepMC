module Data.HepMC.Particle where

import Data.HepMC.LorentzVector
import qualified Data.IntMap as IM

data Particle = Particle {
    partBarcode :: Int,
    pdgID :: Int,
    partFourVec :: XYZT,
    partM :: Double,
    partStatus :: Int,
    polarizationTheta :: Double,
    polarizationPhi :: Double,
    parentVertexBarcode :: Int,
    nFlows :: Int,
    flows :: [(Int, Int)]
} deriving (Eq, Ord, Read, Show)

type Particles = IM.IntMap Particle

module Data.HepMC.Particle where

import Data.HepMC.FourMomentum
import Data.HepMC.Barcode
import qualified Data.Set as S

data Particle = Particle {
    partBC :: BC,
    pdgID :: Int,
    partFourVec :: XYZT,
    partM :: Double,
    partStatus :: Int,
    polarizationTheta :: Double,
    polarizationPhi :: Double,
    parentVertexBarcode :: Int,
    nFlows :: Int,
    flows :: [(Int, Int)]
} deriving (Read, Show)

instance Barcode Particle where
    bc p = partBC p

instance Eq Particle where
    (==) = liftBC2 (==)

instance Ord Particle where
    compare = liftBC2 compare


type Particles = S.Set Particle

class HasParticles hp where
    particles :: hp -> Particles

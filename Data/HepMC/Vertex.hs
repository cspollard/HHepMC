module Data.HepMC.Vertex where

import Data.HepMC.FourMomentum
import Data.HepMC.Barcode
import Data.HepMC.Particle
import qualified Data.Set as S

data Vertex = Vertex {
    vtxBC :: Int,
    vtxID :: Int,
    vtxFourVec :: XYZT,
    nOrphan :: Int,
    nOutgoing :: Int,
    nVtxWeights :: Int,
    vtxWeights :: [Double],
    vtxParts :: Particles
} deriving (Read, Show)

instance Barcode Vertex where
    bc = vtxBC

instance Eq Vertex where
    (==) = liftBC2 (==)

instance Ord Vertex where
    compare = liftBC2 compare

instance HasParticles Vertex where
    particles = vtxParts

type Vertices = S.Set Vertex

class HasVertices hp where
    vertices :: hp -> Vertices

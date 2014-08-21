module Data.HepMC.Vertex where

import Data.HepMC.LorentzVector
import Data.HepMC.Particle

data Vertex = Vertex {
    vertexBarcode :: Int,
    vertexID :: Int,
    vertexFourVec :: XYZT,
    nOrphan :: Int,
    nOutgoing :: Int,
    nVertexWeights :: Int,
    vertexWeights :: [Double],
    particles :: [Particle]
} deriving (Eq, Ord, Read, Show)

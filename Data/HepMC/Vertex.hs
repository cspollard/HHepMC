module Data.HepMC.Vertex where

import Data.HepMC.LorentzVector
import qualified Data.IntMap as IM

data Vertex = Vertex {
    vertexBarcode :: Int,
    vertexID :: Int,
    vertexFourVec :: XYZT,
    nOrphan :: Int,
    nOutgoing :: Int,
    nVertexWeights :: Int,
    vertexWeights :: [Double],
    particles :: [Int]
} deriving (Eq, Ord, Read, Show)

type Vertices = IM.IntMap Vertex

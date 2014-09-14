module Data.HepMC.Vertex where

import Data.HepMC.LorentzVector
import qualified Data.Set as S

data Vertex = Vertex {
    vtxBC :: Int,
    vtxID :: Int,
    vtxFourVec :: XYZT,
    nOrphan :: Int,
    nOutgoing :: Int,
    nVtxWeights :: Int,
    vtxWeights :: [Double],
    particles :: [Int]
} deriving (Read, Show)

instance Barcode Vertex where
    bc p = vtxBC p

instance Eq Vertex where
    (==) = liftBC2 (==)

instance Ord Vertex where
    compare = liftBC2 compare


type Vertices = S.Set Vertex


class HasVertices hp where
    vertices :: hp -> Vertices

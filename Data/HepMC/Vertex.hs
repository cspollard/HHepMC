module Data.HepMC.Vertex where

import Data.HepMC.XYZT
import Data.HepMC.FourMomentum

data Vertex = Vertex {
    vtxID :: Int,
    vtxFourMom :: XYZT,
    vtxNOrphan :: Int,
    vtxNOutgoing :: Int,
    vtxNWeights :: Int,
    vtxWeights :: [Double]
} deriving (Read, Show)

type Vertices = [Vertex]

instance HasFourMom Vertex where
    fourMom = vtxFourMom

instance FourMomentum Vertex where
    xV = xV . fourMom
    yV = yV . fourMom
    zV = zV . fourMom
    tV = tV . fourMom

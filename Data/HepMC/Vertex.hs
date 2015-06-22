module Data.HepMC.Vertex where

import Data.HepMC.PID
import Data.HepMC.XYZT
import Data.HepMC.FourMomentum
import Data.HepMC.Barcoded


data Vertex = Vertex {
    vertBC :: BC,
    vertID :: Int,
    vertFourMom :: XYZT,
    vertNOrphan :: Int,
    vertNOutgoing :: Int,
    vertNWeights :: Int,
    vertWeights :: [Double],
    vertParentParts :: Particles,
    vertChildParts :: Particles
} deriving (Read, Show)

data Particle = Particle {
    partBC :: BC,
    partPID :: Int,
    partFourMom :: XYZT,
    partM :: Double,
    partStatus :: Int,
    partPolarizationTheta :: Double,
    partPolarizationPhi :: Double,
    partNFlows :: Int,
    partFlows :: [(Int, Int)],
    partParentVerts :: Vertices,
    partChildVerts :: Vertices
} deriving (Read, Show)


type Vertices = [Vertex]

instance Barcoded Vertex where
    bc = vertBC

instance Eq Vertex where
    (==) = liftBC2 (==)

instance Ord Vertex where
    compare = liftBC2 compare

instance HasFourMom Vertex where
    fourMom = vertFourMom

instance FourMomentum Vertex where
    xV = xV . fourMom
    yV = yV . fourMom
    zV = zV . fourMom
    tV = tV . fourMom


type Particles = [Particle]

instance Barcoded Particle where
    bc = partBC

instance Eq Particle where
    (==) = liftBC2 (==)

instance Ord Particle where
    compare = liftBC2 compare

instance HasFourMom Particle where
    fourMom = partFourMom

instance FourMomentum Particle where
    xV = xV . fourMom
    yV = yV . fourMom
    zV = zV . fourMom
    tV = tV . fourMom

instance HasPID Particle where
    pid = partPID

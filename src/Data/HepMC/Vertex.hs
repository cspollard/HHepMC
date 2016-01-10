module Data.HepMC.Vertex where

import Data.HEP.PID
import Data.HEP.LorentzVector
import Data.HepMC.Barcoded


data Vertex = Vertex {
    vertBC :: Int,
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
    partBC :: Int,
    partPID :: Int,
    partFourMom :: XYZT,
    partM :: Double,
    partStatus :: Int,
    partPolarizationTheta :: Double,
    partPolarizationPhi :: Double,
    partFlows :: [(Int, Int)],
    partParentVert :: Vertex,
    partChildVert :: Vertex
} deriving (Read, Show)


type Vertices = [Vertex]

class HasVertices hv where
    vertices :: hv -> Vertices

instance Barcoded Vertex where
    bc = vertBC

instance Eq Vertex where
    (==) = liftBC2 (==)

instance Ord Vertex where
    compare = liftBC2 compare

instance HasLorentzVector Vertex where
    lv = fromLV . vertFourMom

type Particles = [Particle]

instance Barcoded Particle where
    bc = partBC

instance Eq Particle where
    (==) = liftBC2 (==)

instance Ord Particle where
    compare = liftBC2 compare

instance HasLorentzVector Particle where
    lv = fromLV . partFourMom

instance HasPID Particle where
    pid = partPID

class HasParticles hp where
    particles :: hp -> Particles
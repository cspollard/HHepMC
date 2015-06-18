module Data.HepMC.Vertex where

import Data.HepMC.XYZT
import Data.HepMC.FourMomentum
import Data.HepMC.PID

data Vertex = Vertex {
    vtxID :: Int,
    vtxFourMom :: XYZT,
    vtxNOrphan :: Int,
    vtxNOutgoing :: Int,
    vtxNWeights :: Int,
    vtxWeights :: [Double],
    vtxParentParts :: Particles,
    vtxChildParts :: Particles
} deriving (Read, Show)

type Vertices = [Vertex]

instance HasFourMom Vertex where
    fourMom = vtxFourMom

instance FourMomentum Vertex where
    xV = xV . fourMom
    yV = yV . fourMom
    zV = zV . fourMom
    tV = tV . fourMom


data Particle = Particle {
    partPID :: Int,
    partFourMom :: XYZT,
    partM :: Double,
    partStatus :: Int,
    partPolarizationTheta :: Double,
    partPolarizationPhi :: Double,
    partNFlows :: Int,
    partFlows :: [(Int, Int)],
    partParentVtx :: Vertex,
    partChildVtx :: Maybe Vertex
} deriving (Read, Show)

type Particles = [Particle]

instance HasFourMom Particle where
    fourMom = partFourMom

instance FourMomentum Particle where
    xV = xV . fourMom
    yV = yV . fourMom
    zV = zV . fourMom
    tV = tV . fourMom

instance HasPID Particle where
    pid = partPID



{-
instance Barcoded Particle where
    bc = partBC


-- same barcode -> same particle
instance Eq Particle where
    (==) = liftBC2 (==)



class HasParticles hp where
    particles :: hp -> Particles

-}

{-

-- TODO
-- convenience functions
-- children :: Particle -> Particles
-- childred = partParentVtxBC 


-- TODO
-- Do we need the BC?


instance Barcoded Vertex where
    bc = vtxBC


instance Eq Vertex where
    (==) = liftBC2 (==)


instance Ord Vertex where
    compare = liftBC2 compare





class HasHepMCVertices hp where
    vertices :: hp -> Vertices



parserVertexInfo :: Parser (Particles -> Vertices -> Vertex)
-}

module Data.HepMC.Vertex where

import Data.HepMC.XYZT
import Data.HepMC.FourMomentum
import Data.HepMC.PID

data Particle = Particle {
    partPID :: Int,
    partFourMom :: XYZT,
    partM :: Double,
    partStatus :: Int,
    partPolarizationTheta :: Double,
    partPolarizationPhi :: Double,
    partNFlows :: Int,
    partFlows :: [(Int, Int)]
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

module Data.HepMC.Particle where

import Data.IntMap (IntMap)
import Data.HepMC.Parse
import Data.HepMC.FourMomentum
import Data.HepMC.XYZT
import Data.HepMC.Barcoded
import Data.HepMC.PID

data Particle = Particle {
    partBC :: BC,
    pdgID :: Int,
    partFourMom :: XYZT,
    partM :: Double,
    partStatus :: Int,
    polarizationTheta :: Double,
    polarizationPhi :: Double,
    parentVertexBC :: Int,
    nFlows :: Int,
    flows :: [(Int, Int)]
} deriving (Read, Show)


instance Barcoded Particle where
    bc = partBC


-- same barcode -> same particle
instance Eq Particle where
    (==) = liftBC2 (==)


instance HasPID Particle where
    pid = pdgID


type Particles = IntMap Particle

class HasParticles hp where
    particles :: hp -> Particles


instance FourMomentum Particle where
    xV = xV . partFourMom
    yV = yV . partFourMom
    zV = zV . partFourMom
    tV = tV . partFourMom


parserParticle :: Parser Particle
parserParticle = do
    _ <- char 'P' <* skipSpace
    bcode <- decSpace
    pdgid <- decSpace


    mom <- parseXYZT <* skipSpace

    m <- doubSpace
    stat <- decSpace
    ptheta <- doubSpace
    pphi <- doubSpace
    pvbc <- decSpace
    nf <- decSpace

    fs <- count nf $ tuple decSpace decSpace

    return $
        Particle bcode pdgid mom m stat ptheta pphi pvbc nf fs

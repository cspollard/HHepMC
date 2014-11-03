module Data.HepMC.Particle where

import Data.HepMC.FourMomentum
import Data.HepMC.Barcoded
import qualified Data.Set as S

data Particle = Particle {
    partBC :: BC,
    pdgID :: Int,
    partFourVec :: XYZT,
    partM :: Double,
    partStatus :: Int,
    polarizationTheta :: Double,
    polarizationPhi :: Double,
    parentVertexBarcode :: Int,
    nFlows :: Int,
    flows :: [(Int, Int)]
} deriving (Read, Show)

instance Barcoded Particle where
    bc p = partBC p

instance Eq Particle where
    (==) = liftBC2 (==)

instance Ord Particle where
    compare = liftBC2 compare


type Particles = S.Set Particle

class HasParticles hp where
    particles :: hp -> Particles


parseParticle :: Parser Particle
parseParticle = do
    char 'P'; skipSpace
    bc <- dec
    pdgid <- dec

    x <- doub
    y <- doub
    z <- doub
    t <- doub

    let vec = XYZT x y z t

    m <- doub
    stat <- dec
    ptheta <- doub
    pphi <- doub
    pvbc <- dec
    nf <- dec
    
    let f = do
        s <- dec
        t <- dec
        return (s, t)

    fs <- parseList nf f

    return $
        Particle bc pdgid vec m stat ptheta pphi pvbc nf fs

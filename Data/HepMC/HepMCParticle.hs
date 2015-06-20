module Data.HepMC.HepMCVertex where

import Data.HepMC.Parse
import Data.HepMC.Barcoded
import Data.HepMC.Particle
import Data.HepMC.XYZT

-- temporary type that encodes all hepmc info before building event graph
data HepMCParticle = HepMCParticle {
    hpartBC :: BC,
    hpartPID :: Int,
    hpartPX :: Double,
    hpartPY :: Double,
    hpartPZ :: Double,
    hpartE :: Double,
    hpartM :: Double,
    hpartStatus :: Int,
    hpartPolarizationTheta :: Double,
    hpartPolarizationPhi :: Double,
    hpartChildVtxBC :: Int,
    hpartNFlows :: Int,
    hpartFlows :: [(Int, Int)]
} deriving (Read, Show)


instance Barcoded HepMCParticle where
    bc = hpartBC

instance Eq HepMCParticle where
    (==) = liftBC2 (==)

instance Ord HepMCParticle where
    compare = liftBC2 compare


toParticle :: HepMCParticle -> Particle
toParticle p =
        Particle (hpartPID p)
            (XYZT (hpartPX p) (hpartPY p) (hpartPZ p) (hpartE p))
            (hpartM p) (hpartStatus p)
            (hpartPolarizationTheta p)
            (hpartPolarizationPhi p)
            (hpartNFlows p)
            (hpartFlows p)


parserHepMCParticle :: Parser HepMCParticle
parserHepMCParticle = do
    _ <- char 'P' *> skipSpace

    bcode <- decSpace
    pdgid <- decSpace

    px <- doubSpace
    py <- doubSpace
    pz <- doubSpace
    e <- doubSpace

    m <- doubSpace
    stat <- decSpace
    ptheta <- doubSpace
    pphi <- doubSpace
    cvbc <- decSpace
    nf <- decSpace

    fs <- count nf $ tuple decSpace decSpace

    return $ HepMCParticle bcode pdgid px py pz e m stat ptheta pphi cvbc nf fs

module Data.HepMC.HepMCVertex where

import Data.HepMC.Parse
import Data.HepMC.Barcoded
import Data.HepMC.Vertex
import Data.HepMC.XYZT

-- temporary types that encode all hepmc info before building event
-- graph
data HepMCVertex = HepMCVertex {
    hvtxBC :: BC,
    hvtxID :: Int,
    hvtxX :: Double,
    hvtxY :: Double,
    hvtxZ :: Double,
    hvtxCTau :: Double,
    hvtxNOrphan :: Int,
    hvtxNOutgoing :: Int,
    hvtxNWeights :: Int,
    hvtxWeights :: [Double]
} deriving (Read, Show)


instance Barcoded HepMCVertex where
    bc = hvtxBC

instance Eq HepMCVertex where
    (==) = liftBC2 (==)

instance Ord HepMCVertex where
    compare = liftBC2 compare


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


toVertex :: HepMCVertex -> Vertex
toVertex v = 
        Vertex (hvtxID v)
            (XYZT (hvtxX v) (hvtxY v) (hvtxZ v) (hvtxCTau v))
            (hvtxNOrphan v) (hvtxNOutgoing v)
            (hvtxNWeights v) (hvtxWeights v)


parserHepMCVertex :: Parser HepMCVertex
parserHepMCVertex = do
    _ <- char 'V' <* skipSpace
    vtxbc <- decSpace
    vtxid <- decSpace

    x <- doubSpace
    y <- doubSpace
    z <- doubSpace
    ctau <- doubSpace

    norph <- decSpace
    nout <- decSpace
    nvtxwgt <- decSpace
    vtxwgts <- count nvtxwgt doubSpace

    return $ HepMCVertex vtxbc vtxid x y z ctau norph nout nvtxwgt vtxwgts


toParticle :: HepMCParticle -> Particle
toParticle p =
        Particle (hpartPID p)
            (XYZT (hpartPX p) (hpartPY p) (hpartPZ p) (hpartE p))
            (hpartM p) (hpartStatus p)
            (hpartPolarizationTheta p)
            (hpartPolarizationPhi p)
            (hpartNFlows p)
            (hpartFlows p)


-- TODO
-- should this just parse to
-- Vector Vertices -> Particle ??
-- how then would we deal with the particle -> vertex link?
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

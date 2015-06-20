module Data.HepMC.HepMCVertex where

import Data.HepMC.Parse
import Data.HepMC.Barcoded
import Data.HepMC.Vertex
import Data.HepMC.XYZT

-- temporary type that encodes all hepmc info before building event graph
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

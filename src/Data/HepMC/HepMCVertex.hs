module Data.HepMC.HepMCVertex where

import Data.HepMC.Parse
import Data.HepMC.Barcoded
import Data.HepMC.Vertex
import Data.HepMC.XYZT

-- temporary type that encodes all hepmc info before building event graph
data HepMCVertex = HepMCVertex {
    hvertBC :: BC,
    hvertID :: Int,
    hvertX :: Double,
    hvertY :: Double,
    hvertZ :: Double,
    hvertCTau :: Double,
    hvertNOrphan :: Int,
    hvertNOutgoing :: Int,
    hvertNWeights :: Int,
    hvertWeights :: [Double]
} deriving (Read, Show)


instance Barcoded HepMCVertex where
    bc = hvertBC

instance Eq HepMCVertex where
    (==) = liftBC2 (==)

instance Ord HepMCVertex where
    compare = liftBC2 compare


toVertex :: HepMCVertex -> Particles -> Particles -> Vertex
toVertex v = 
        Vertex (hvertBC v) (hvertID v)
            (XYZT (hvertX v) (hvertY v) (hvertZ v) (hvertCTau v))
            (hvertNOrphan v) (hvertNOutgoing v)
            (hvertNWeights v) (hvertWeights v)


parserHepMCVertex :: Parser HepMCVertex
parserHepMCVertex = do
    _ <- char 'V' <* skipSpace
    vertbc <- decSpace
    vertid <- decSpace

    x <- doubSpace
    y <- doubSpace
    z <- doubSpace
    ctau <- doubSpace

    norph <- decSpace
    nout <- decSpace
    nvertwgt <- decSpace
    vertwgts <- count nvertwgt doubSpace

    return $ HepMCVertex vertbc vertid x y z ctau norph nout nvertwgt vertwgts

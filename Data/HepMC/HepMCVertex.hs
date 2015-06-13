module Data.HepMC.HepMCVertex where

import Data.HepMC.Parse
import Data.HepMC.XYZT
import Data.HepMC.FourMomentum
import Data.HepMC.Barcoded
import Data.HepMC.Particle
import Data.IntMap (IntMap, keys)


data HepMCVertex = HepMCVertex {
    vtxBC :: Int,
    vtxID :: Int,
    vtxFourVec :: XYZT,
    vtxNOrphan :: Int,
    vtxNOutgoing :: Int,
    vtxNWeights :: Int,
    vtxWeights :: [Double],
    vtxPartBCs :: [Int]
} deriving (Read, Show)


instance Barcoded HepMCVertex where
    bc = vtxBC


instance Eq HepMCVertex where
    (==) = liftBC2 (==)


instance Ord HepMCVertex where
    compare = liftBC2 compare


instance FourMomentum HepMCVertex where
    xV = xV . vtxFourVec
    yV = yV . vtxFourVec
    zV = zV . vtxFourVec
    tV = tV . vtxFourVec


type HepMCVertices = IntMap HepMCVertex


class HasHepMCVertices hp where
    vertices :: hp -> HepMCVertices


parserVertParts :: Parser (HepMCVertex, Particles)
parserVertParts = do
    _ <- char 'V' <* skipSpace
    vtxbc <- decSpace
    vtxid <- decSpace

    mom <- parserXYZT <* skipSpace

    norph <- decSpace
    nout <- decSpace
    nvtxwgt <- decSpace
    vtxwgts <- count nvtxwgt doubSpace

    parts <- fromListBC <$> many parserParticle

    -- TODO
    -- find bcs twice; only need to once
    let v = HepMCVertex vtxbc vtxid mom norph nout nvtxwgt vtxwgts $ keys parts

    return (v, parts)

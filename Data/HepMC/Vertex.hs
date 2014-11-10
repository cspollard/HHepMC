module Data.HepMC.Vertex where

import Data.HepMC.Parser
import Data.HepMC.XYZT
import Data.HepMC.FourMomentum
import Data.HepMC.Barcoded
import Data.HepMC.Particle
import Data.IntMap (IntMap, fromList, keys)


data Vertex = Vertex {
    vtxBC :: Int,
    vtxID :: Int,
    vtxFourVec :: XYZT,
    nOrphan :: Int,
    nOutgoing :: Int,
    nVtxWeights :: Int,
    vtxWeights :: [Double],
    vtxPartBCs :: [Int]
} deriving (Read, Show)


instance Barcoded Vertex where
    bc = vtxBC


instance Eq Vertex where
    (==) = liftBC2 (==)


instance Ord Vertex where
    compare = liftBC2 compare


instance FourMomentum Vertex where
    xV = xV . vtxFourVec
    yV = yV . vtxFourVec
    zV = zV . vtxFourVec
    tV = tV . vtxFourVec


type Vertices = IntMap Vertex


class HasVertices hp where
    vertices :: hp -> Vertices


parserVertParts :: Parser (Vertex, Particles)
parserVertParts = do
    _ <- char 'V' <* skipSpace
    vtxbc <- decSpace
    vtxid <- decSpace

    mom <- parseXYZT <* skipSpace

    norph <- decSpace
    nout <- decSpace
    nvtxwgt <- decSpace
    vtxwgts <- count nvtxwgt doubSpace

    parts <- fromList . map (\p -> (bc p, p)) <$> many parserParticle

    -- TODO
    -- find bcs twice; only need to once
    let v = Vertex vtxbc vtxid mom norph nout nvtxwgt vtxwgts $ keys parts

    return (v, parts)

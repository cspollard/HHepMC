module Data.HepMC.Vertex where

import Data.HepMC.Parser.Common
import Data.HepMC.XYZT
import Data.HepMC.FourMomentum
import Data.HepMC.Barcoded
import Data.HepMC.Particle
import qualified Data.Set as S

data Vertex = Vertex {
    vtxBC :: Int,
    vtxID :: Int,
    vtxFourVec :: XYZT,
    nOrphan :: Int,
    nOutgoing :: Int,
    nVtxWeights :: Int,
    vtxWeights :: [Double],
    vtxParts :: Particles
} deriving (Read, Show)


instance Barcoded Vertex where
    bc = vtxBC


instance Eq Vertex where
    (==) = liftBC2 (==)


instance Ord Vertex where
    compare = liftBC2 compare


instance HasParticles Vertex where
    particles = vtxParts


instance FourMomentum Vertex where
    xV = xV . vtxFourVec
    yV = yV . vtxFourVec
    zV = zV . vtxFourVec
    tV = tV . vtxFourVec


type Vertices = S.Set Vertex


class HasVertices hp where
    vertices :: hp -> Vertices


parserVertex :: Parser Vertex
parserVertex = do
    _ <- char 'V' <* skipSpace
    vtxbc <- decSpace
    vtxid <- decSpace

    mom <- parseXYZT

    norph <- decSpace
    nout <- decSpace
    nvtxwgt <- decSpace
    vtxwgts <- count nvtxwgt doubSpace

    parts <- S.fromList <$> many parserParticle

    return $ Vertex vtxbc vtxid mom norph nout nvtxwgt vtxwgts parts

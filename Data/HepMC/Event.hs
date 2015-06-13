module Data.HepMC.Event where

import Data.IntMap (insert, union)
import qualified Data.IntMap as IM (empty)

import Data.HepMC.Parse
import Data.HepMC.EventHeader
import Data.HepMC.Barcoded
import Data.HepMC.HepMCVertex
import Data.HepMC.HepMCParticle


data Event = Event {
    eventHeader :: EventHeader,
    eventVertices :: Vertices,
    eventParticles :: Particles
} deriving (Read, Show)


instance HasParticles Event where
    particles = eventParticles

instance HasVertices Event where
    vertices = eventVertices

parserEvent :: Parser Event
parserEvent = do
    header <- parserEventHeader

    let f (v, ps') (vs, ps) = (insert (bc v) v vs, ps `union` ps')
    (verts, parts) <- foldr f (IM.empty, IM.empty) <$> many parserVertParts
    
    return $ Event header verts parts

-- This appears to be what I want:
data HepMCEventGraph = HepMCEventGraph HepMCVertex

data HepMCParticle = HepMCParticle HepMCVertices

type HepMCVertices = [HepMCVertex]
type HepMCParticles = [HepMCParticle]

mkGraph :: [(a, Int)] -> Graph a
mkGraph table = table' ! 0
    where table' = listArray (0, length table - 1) $
            map (\(x, n) -> GNode x (table' ! n)) table

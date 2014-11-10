module Data.HepMC.Event where

import Data.IntMap (insert, union)
import qualified Data.IntMap as IM (empty)

import Data.HepMC.Parser.Common
import Data.HepMC.EventHeader
import Data.HepMC.Barcoded
import Data.HepMC.Vertex
import Data.HepMC.Particle


data Event = Event {
    eventHeader :: EventHeader,
    eventVertices :: Vertices,
    eventParticles :: Particles
} deriving (Eq, Ord, Read, Show)


parserEvent :: Parser Event
parserEvent = do
    header <- parserEventHeader

    let f (v, ps') (vs, ps) = (insert (bc v) v vs, ps `union` ps')
    (verts, parts) <- foldr f (IM.empty, IM.empty) <$> many parserVertParts
    
    return $ Event header verts parts

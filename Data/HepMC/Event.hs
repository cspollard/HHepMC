module Data.HepMC.Event where

import Data.HepMC.Parse
import Data.HepMC.EventHeader
import Data.HepMC.Barcoded
import Data.HepMC.Vertex
import Data.HepMC.Particle


data Event = Event {
    evtHeader :: EventHeader,
    evtVertices :: Vertices,
    evtParticles :: Particles
} deriving (Read, Show)


instance HasParticles Event where
    particles = evtParticles

instance HasVertices Event where
    vertices = evtVertices

parserEvent :: Parser Event
parserEvent = do
    header <- parserEventHeader

    let f (v, ps') (vs, ps) = (insert (bc v) v vs, ps `union` ps')
    (verts, parts) <- foldr f (IM.empty, IM.empty) <$> many parserVertParts
    
    return $ Event header verts parts

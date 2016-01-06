module Data.HepMC.GenEvent where

import Data.HepMC.Units
import Data.HepMC.Vertex
import qualified Data.Set as S

data GenEvent = GenEvent {
    momentumUnit :: UnitMomentum,
    lengthUnit :: UnitLength,
    eventVertices :: Vertices
} deriving (Read, Show)

instance HasVertices GenEvent where
    vertices = eventVertices

instance HasParticles GenEvent where
    -- TODO
    -- slow?
    -- union of sets of particles from each vertex in event
    particles = S.toAscList . S.unions . map particles . S.toAscList . vertices

genEvent :: UnitMomentum -> UnitLength -> GenEvent
genEvent m l = GenEvent m l []

-- addVertex :: Vertex -> GenEvent -> GenEvent
-- addVertex v g = g {eventVertices = S.insert v (vertices g)}

-- removeVertex :: Vertex -> GenEvent -> GenEvent
-- removeVertex v g = g {eventVertices = S.delete v (vertices g)}

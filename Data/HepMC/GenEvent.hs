module Data.HepMC.GenEvent where

import Data.HepMC.Units
import Data.HepMC.Particle
import Data.HepMC.Vertex
import qualified Data.IntMap as IM

data GenEvent = GenEvent {
    momentumUnit :: UnitMomentum,
    lengthUnit :: UnitLength,
    eventVertices :: Vertices,
    eventParticles :: Particles
} deriving (Read, Show)

instance HasParticles GenEvet where
    particles = eventParticles

instance HasVertices GenEvet where
    vertices = eventVertices

genEvent :: UnitMomentum -> UnitLength -> GenEvent
genEvent m l = GenEvent m l IM.empty IM.empty

addVertex :: GenEvent -> Vertex -> GenEvent
addVertex g v = g {eventVertices = IM.insert (bc v) v (vertices g)}

removeVertex :: GenEvent -> Vertex -> GenEvent
removeVertex g v = g {eventVertices = IM.delete (bc v) vertices g) v}

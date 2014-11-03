module Data.HepMC.Event where

import qualified Data.Text.Lazy as TL
import Data.HepMC.EventHeader
import Data.HepMC.Vertex
import Data.HepMC.Particle


data Event = Event {
    eventHeader :: EventHeader,
    eventVertices :: Vertices,
    eventParticles :: Particles
} deriving (Eq, Ord, Read, Show)

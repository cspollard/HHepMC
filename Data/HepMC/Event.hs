module Data.HepMC.Event where

import qualified Data.Text.Lazy as TL
import Data.Set (fromList)

import Data.HepMC.Parser.Common
import Data.HepMC.EventHeader
import Data.HepMC.Vertex
import Data.HepMC.Particle


data Event = Event {
    eventHeader :: EventHeader,
    eventVertices :: Vertices
    -- eventParticles :: Particles
} deriving (Eq, Ord, Read, Show)


parserEvent :: Parser Event
parserEvent = Event <$> parserEventHeader <*> (fromList <$> many parserVertex)

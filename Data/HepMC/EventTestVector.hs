module Data.HepMC.EventTestVector where

import qualified Data.Vector as V

-- This appears to be what I want:
data EventGraph = EventGraph Vertex deriving Show

data Particle = Particle Vertices deriving Show
data Vertex = Vertex Particles deriving Show

type Vertices = [Vertex]
type Particles = [Particle]

mkEvent :: [[Int]] -> [[Int]] -> EventGraph
mkEvent vertLinks partLinks = EventGraph $ verts V.! 0
    where
        verts = V.fromList $
                map (Vertex . map ((V.!) parts)) vertLinks

        parts = V.fromList $
                map (Particle . map ((V.!) verts)) partLinks

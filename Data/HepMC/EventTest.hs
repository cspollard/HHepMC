module Data.HepMC.EventTest where

import qualified Data.Array as A

-- This appears to be what I want:
data EventGraph = EventGraph Vertex deriving Show

data Particle = Particle Vertices deriving Show
data Vertex = Vertex Particles deriving Show

type Vertices = [Vertex]
type Particles = [Particle]

mkEvent :: [[Int]] -> [[Int]] -> EventGraph
mkEvent vertLinks partLinks = EventGraph $ verts A.! 0
    where
        verts = A.listArray (0, length vertLinks - 1) $
                map (Vertex . map ((A.!) parts)) vertLinks

        parts = A.listArray (0, length partLinks - 1) $
                map (Particle . map ((A.!) verts)) partLinks

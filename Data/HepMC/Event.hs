module Data.HepMC.Event where

import Data.HepMC.Parse
import Data.HepMC.HepMCVertex
import Data.HepMC.EventHeader
import qualified Data.Array as A
import Data.HepMC.Vertex


data Event = Event {
    evtHeader :: EventHeader,
    evtGraph :: EventGraph
} deriving (Show)

parserEvent :: Parser Event
parserEvent = Event <$>
                parserEventHeader <*>
                parserEventGraph

parserEventGraph :: Parser EventGraph
parserEventGraph = makeEventGraph <$> many' parserHepMCVertex



-- TODO
-- direct link to HS vertex?
data EventGraph = EventGraph {
    egVerts :: Vertices
    -- egParts :: Particles
} deriving (Read, Show)

makeEventGraph :: [HepMCVertex] -> EventGraph
makeEventGraph verts = EventGraph . A.elems $ verts'
    where
        -- TODO
        -- -bcode?!
        verts' = let l = map (\v -> toVertex v $ parts v) verts in A.listArray (1, length l) l
        toPart v p = toParticle p ( (let bcode = hpartChildVtxBC p in if bcode /= 0 then Just $ verts' A.! (-bcode) else Nothing) )
        parts v = map (toPart $ verts' A.! (negate $ hvtxBC v) ) $ hvtxParts v

{-
instance HasParticles Event where
    particles = evtParticles

instance HasVertices Event where
    vertices = evtVertices
-}

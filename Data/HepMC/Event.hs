module Data.HepMC.Event where

import Data.HepMC.Parse
import Data.HepMC.HepMCVertex
import Data.HepMC.EventHeader
import Data.HepMC.Barcoded
import Data.Array
import Data.Tuple (swap)
import Data.HepMC.Vertex
import qualified Data.Foldable as F


data Event = Event {
    evtHeader :: EventHeader,
    evtGraph :: EventGraph
} deriving (Show)

parserEvent :: Parser Event
parserEvent = Event <$> parserEventHeader <*> parserEventGraph

parserEventGraph :: Parser EventGraph
parserEventGraph = makeEventGraph <$> many' parserHepMCVertex


-- TODO
-- direct link to HS vertex?
data EventGraph = EventGraph {
    egVerts :: Vertices,
    egParts :: Particles
} deriving (Read, Show)


makeEventGraph :: [HepMCVertex] -> EventGraph
makeEventGraph hmcverts = EventGraph (elems vertsarr) (elems partsarr)
    where

        barcodeIt b = (bc b, b)

        -- array of Vertex
        vertsarr :: Array Int Vertex
        vertsarr = let l = map toVertex' hmcverts in
                    listArray (-1, negate $ length l) l

        -- array of Particle
        partsarr :: Array Int Particle
        partsarr = let l = concatMap vtxChildParts $ elems vertsarr in
                    listArray (1, length l) l


        toParticle' :: HepMCParticle -> Vertex -> Particle
        toParticle' p v = toParticle p v childVtx
            where
                childVtx = let vbc = partchildlinksarr ! bc p in
                                if vbc /= 0
                                    then Just $ vertsarr ! vbc
                                    else Nothing


        toVertex' :: HepMCVertex -> Vertex
        toVertex' v = toVertex v (map (partsarr !) (vertparentlinksarr ! bc v)) (childParts v)

        childParts :: HepMCVertex -> Particles
        childParts v = map (flip toParticle' $ vertsarr ! bc v) $ hvtxChildParts v


        -- array of all HepMCVertices
        hmcvertsarr = array (-(length hmcverts), -1) $
                        map barcodeIt hmcverts

        -- array of all HepMCParticles
        hmcpartsarr = array (1, length l) $ map barcodeIt l
            where l = F.concat . fmap hvtxChildParts $ hmcvertsarr

        -- links from particles to child vertices
        partchildlinksarr = fmap hpartChildVtxBC hmcpartsarr

        -- links from vertices to parent particles
        vertparentlinksarr = accumArray (flip (:)) [] (bounds hmcvertsarr) .
                                map swap . assocs $ partchildlinksarr


{-
instance HasParticles Event where
    particles = evtParticles

instance HasVertices Event where
    vertices = evtVertices
-}

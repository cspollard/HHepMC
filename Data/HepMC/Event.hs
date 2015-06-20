module Data.HepMC.Event where

import Data.HepMC.Parse
import Data.HepMC.EventHeader
import Data.HepMC.HepMCVertex
import Data.HepMC.HepMCParticle
import Data.HepMC.Barcoded
import Data.HepMC.Vertex
import Data.HepMC.Particle
import Data.Either
import Data.ABGraph
import Data.List (sortBy, sort)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM


data Event = Event {
    evtHeader :: EventHeader,
    evtGraph :: EventGraph
}

parserEvent :: Parser Event
parserEvent = Event <$> parserEventHeader <*> parserEventGraph

parserEventGraph :: Parser EventGraph
parserEventGraph = makeEventGraph <$> many' (eitherP parserHepMCVertex parserHepMCParticle)


-- TODO
-- direct link to HS vertex?
type EventNode = ABNode Vertex Particle
type EventGraph = ABGraph Vertex Particle

egVerts :: EventGraph -> [Vertex]
egVerts = map aValue . aNodes

egParts :: EventGraph -> [Particle]
egParts = map bValue . bNodes

egFinalParts :: EventGraph -> [Particle]
egFinalParts = map bValue . filter (null . children) . bNodes

egVertNodesMap :: EventGraph -> IntMap EventNode
egVertNodesMap = aNodesMap

egPartNodesMap :: EventGraph -> IntMap EventNode
egPartNodesMap = bNodesMap

type HMCObj = Either HepMCVertex HepMCParticle
type Obj = Either Vertex Particle


-- TODO
-- we could also assemble the list of hverts and hparts here...
-- buildLinks compiles the edges of the event graph
buildLinks :: [HMCObj] -> ([(BC, BC)], [(BC, BC)])
buildLinks hmcobjs = buildLinks' hmcobjs 0 ([], [])
    where
        buildLinks' :: [HMCObj] -> BC -> ([(BC, BC)], [(BC, BC)]) -> ([(BC, BC)], [(BC, BC)])
        buildLinks' [] _ links = links
        buildLinks' (obj:objs) vtxBC links@(vplinks, pvlinks) =
            case obj of
                -- we're at a new vertex.
                Left vert -> buildLinks' objs (bc vert) links
                -- add the vertex -> child part and part -> child vertex links
                Right part -> buildLinks' objs vtxBC
                                ( (vtxBC, bc part) : vplinks,
                                    let bcode = hpartChildVtxBC part in
                                        if bcode == 0
                                            then pvlinks
                                            else (bc part, bcode) : pvlinks)



makeEventGraph :: [HMCObj] -> EventGraph
makeEventGraph hobjs = buildGraph vertsMap partsMap vplinks pvlinks
    where
        -- TODO we have already done this in buildLinks
        (hvertsList, hpartsList) = partitionEithers hobjs

        (vplinks, pvlinks) = buildLinks hobjs

        -- reverse ordering of vertices
        vertsList = map (\v -> (bc v, toVertex v) ) $ sortBy (flip compare) hvertsList
        partsList = map (\p -> (bc p, toParticle p) ) $ sort hpartsList

        vertsMap = IM.fromList vertsList
        partsMap = IM.fromList partsList

module Data.HepMC.EventGraph where

import Data.HepMC.Parse
import qualified Data.Set as S
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.HepMC.Vertex
import Data.HepMC.Barcoded
import qualified Data.HEP.PID as PID
import Data.HEP.PID (HasPID(..))
import Data.Either (partitionEithers)
import Data.Maybe (isNothing, maybeToList)


-- do we need an IntMap of these guys?
data EventGraph = EventGraph {
    egVerts :: Vertices,
    egParts :: Particles,
    egVertsMap :: IntMap Vertex,
    egPartsMap :: IntMap Particle
}


final :: Particle -> Bool
final = isNothing . partChildVert

-- TODO
-- this needs to be looked into.
prompt :: Particle -> Bool
prompt = null . S.filter (\p' -> partStatus p' == 2 && (PID.isHadron p' || PID.isTau p')) . ancestors

parents, children, descendants, ancestors :: Particle -> Particles

parents = vertParentParts . partParentVert
children p = case partChildVert p of
                Nothing -> S.empty
                Just v -> vertChildParts v

descendants = descendants' S.empty
    where
        -- ignore nodes that are already in the set---avoid loops.
        descendants' s n' = if n' `S.member` s
                            then s
                            else foldl descendants' (n' `S.insert` s) (children n')

ancestors n = foldl ancestors' S.empty (parents n)
    where
        -- ignore nodes that are already in the set---avoid loops.
        ancestors' s n' = if n' `S.member` s
                            then s
                            else foldl ancestors' (n' `S.insert` s) (parents n')



{-# LANGUAGE TemplateHaskell #-}


module Data.HepMC.EventGraph where

import Control.Lens hiding (children)

import qualified Data.Set as S
import Data.IntMap (IntMap)
import Data.HepMC.Vertex
import qualified Data.HEP.PID as PID


-- do we need an IntMap of these guys?
data EventGraph =
    EventGraph
        { _egVerts :: Vertices
        , _egParts :: Particles
        , _egVertsMap :: IntMap Vertex
        , _egPartsMap :: IntMap Particle
        } deriving Show

makeLenses ''EventGraph


final :: Particle -> Bool
final = null . view partChildVert

-- TODO
-- this needs to be looked into.
prompt :: Particle -> Bool
prompt = null . S.filter (\p' -> view partStatus p' == 2 && (PID.isHadron p' || PID.isTau p')) . ancestors

parents, children, descendants, ancestors :: Particle -> Particles

parents = view vertParentParts . view partParentVert
children p = case view partChildVert p of
                Nothing -> S.empty
                Just v -> view vertChildParts v

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

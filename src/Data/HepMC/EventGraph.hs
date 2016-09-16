module Data.HepMC.EventGraph where

import Control.Lens hiding (children)

import Data.Array

import Data.IntMap (IntMap)
import Data.HepMC.Vertex

import Data.HepMC.Barcoded

import qualified Data.HEP.PID as PID



final :: Particle -> Bool
final p = null (view pgraph p ! view bc p)


-- TODO
-- this needs to be looked into.
prompt :: Particle -> Bool
prompt = null . view (filtered unstable . ancestors)
    where unstable p = view partStatus p == 2 && (PID.isHadron p || PID.isTau p)

parents, children, descendants, ancestors :: Traversal' Particle Particle

parents p = view pgraph' p ! view bc p

view vertParentParts . view partParentVert
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

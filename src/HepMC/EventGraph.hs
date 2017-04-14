module HepMC.EventGraph where

import           Control.Lens        hiding (children)

import           Data.Array          ((!))
import           Data.Maybe          (mapMaybe)

import qualified Data.Graph          as G
import qualified Data.IntMap         as IM

import qualified Data.HEP.PID        as PID
import           HepMC.Barcoded
import           HepMC.Event



final :: Particle -> Bool
final = null . toListOf children

particles :: Traversal' Event Particle
particles = eparts . traverse

vertices :: Traversal' Event Vertex
vertices = everts . traverse


fromHadron :: Particle -> Bool
fromHadron = not . null . toListOf (ancestors . filtered unstable)
    where unstable p = view partStatus p == 2 && PID.isHadron p

parents, children, descendants, ancestors :: Fold Particle Particle

parents = folding $
    \p -> let g' = view (pevent.graph') p
              imp = view (pevent.eparts) p
              vs = g' ! view bc p
              ps = concatMap (g' !) vs
          in  map (imp IM.!) ps

children = folding $
    \p -> let g = view (pevent.graph) p
              imp = view (pevent.eparts) p
              vs = g ! view bc p
              ps = concatMap (g !) vs
          in  map (imp IM.!) ps

descendants = folding $
    -- TODO
    -- this is inherently inefficient because it looks up vertices
    -- unnecessarily
    \p -> let g = view (pevent.graph) p
              im = view (pevent.eparts) p
              i = view bc p
          -- tail of reachables since the current node is always the
          -- first one listed
          in  mapMaybe (`IM.lookup` im) (tail $ G.reachable g i)

ancestors = folding $
    -- TODO
    -- this is inherently inefficient because it looks up vertices
    -- unnecessarily
    \p -> let g = view (pevent.graph') p
              im = view (pevent.eparts) p
              i = view bc p
          -- tail of reachables since the current node is always the
          -- first one listed
          in  mapMaybe (`IM.lookup` im) (tail $ G.reachable g i)

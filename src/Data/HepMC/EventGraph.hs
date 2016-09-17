module Data.HepMC.EventGraph where

import Control.Lens hiding (children)

import Data.Array ((!))
import Data.Maybe (mapMaybe)

import qualified Data.Graph as G
import qualified Data.IntMap as IM

import qualified Data.HEP.PID as PID
import Data.HepMC.Barcoded
import Data.HepMC.Event



final :: Particle -> Bool
final = null . view children

particles :: Traversal' Event Particle
particles = eparts . traverse

vertices :: Traversal' Event Vertex
vertices = everts . traverse


prompt :: Particle -> Bool
prompt = null . view (filtered unstable . ancestors)
    where unstable p = view partStatus p == 2 && (PID.isHadron p || PID.isTau p)

-- TODO
-- make these traversals
parents, children, descendants, ancestors :: Monoid r => Getting r Particle [Particle]

parents = to $
    \p -> let g' = view (pevent.graph') p
              imp = view (pevent.eparts) p
              vs = g' ! view bc p
              ps = concatMap (g' !) vs
          in  map (imp IM.!) ps

children = to $
    \p -> let g = view (pevent.graph) p
              imp = view (pevent.eparts) p
              vs = g ! view bc p
              ps = concatMap (g !) vs
          in  map (imp IM.!) ps

descendants = to $
    -- TODO
    -- this is inherently inefficient because it looks up vertices
    -- unnecessarily
    \p -> let g = view (pevent.graph) p
              im = view (pevent.eparts) p
              i = view bc p
          -- tail of reachables since the current node is always the
          -- first one listed
          in  mapMaybe (`IM.lookup` im) (tail $ G.reachable g i)

ancestors = to $
    -- TODO
    -- this is inherently inefficient because it looks up vertices
    -- unnecessarily
    \p -> let g = view (pevent.graph') p
              im = view (pevent.eparts) p
              i = view bc p
          -- tail of reachables since the current node is always the
          -- first one listed
          in  mapMaybe (`IM.lookup` im) (tail $ G.reachable g i)

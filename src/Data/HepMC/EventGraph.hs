module Data.HepMC.EventGraph where

import Control.Lens hiding (children)

import Data.Maybe (mapMaybe)

import qualified Data.Graph as G
import qualified Data.IntMap as IM

import qualified Data.HEP.PID as PID
import Data.HepMC.Barcoded
import Data.HepMC.Event



final :: Particle -> Bool
final = null . view pchildren

particles :: Traversal' Event Particle
particles = eparts . traverse

vertices :: Traversal' Event Vertex
vertices = everts . traverse

-- TODO
-- this needs to be looked into.
prompt :: Particle -> Bool
prompt = null . view (filtered unstable . ancestors)
    where unstable p = view partStatus p == 2 && (PID.isHadron p || PID.isTau p)

-- NB:
-- traversals don't seem safe in self-referential structures...
parents, children, descendants, ancestors :: Monoid r => Getting r Particle [Particle]

parents = pparents . traverse . vparents
children = pchildren . traverse . vchildren

descendants = to $
    -- TODO
    -- this is inherently inefficient because it looks up vertices
    -- unnecessarily
    \p -> let g = view (pevent . graph) p
              im = view (pevent . eparts) p
              i = view bc p
          in  mapMaybe (`IM.lookup` im) (G.reachable g i)

ancestors = to $
    -- TODO
    -- this is inherently inefficient because it looks up vertices
    -- unnecessarily
    \p -> let g = view (pevent . graph') p
              im = view (pevent . eparts) p
              i = view bc p
          in  mapMaybe (`IM.lookup` im) (G.reachable g i)

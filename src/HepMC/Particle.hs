{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module HepMC.Particle
    ( Vertex (Vertex)
    , vertNOrphan, vertNOutgoing, vertWeights
    , Particle (Particle)
    , partM, partStatus, partPolarizationTheta, partPolarizationPhi, partFlows
    , final, fromHadron
    , parents, children, ancestors, descendants
    ) where

import           Control.Lens           hiding (children)
import           Data.Array             ((!))
import           Data.Function
import qualified Data.Graph             as G
import           Data.HEP.LorentzVector
import           Data.HEP.PID
import qualified Data.IntMap.Strict     as IM
import           Data.Maybe             (mapMaybe)
import           Data.Vector            (Vector)
import           HepMC.Barcoded
import           HepMC.EventGraph
import           HepMC.Internal



data Vertex =
  Vertex
    { _vgraph  :: EventGraph
    , _rawvert :: RawVertex
    }


data Particle =
  Particle
    { _pgraph  :: EventGraph
    , _rawpart :: RawParticle
    }

makeLenses ''Vertex
makeLenses ''Particle

instance Eq Vertex where
  (==) = (==) `on` view rawvert

instance Eq Particle where
  (==) = (==) `on` view rawpart

instance Ord Vertex where
  compare = compare `on` view rawvert

instance Ord Particle where
  compare = compare `on` view rawpart

instance Show Vertex where
  show = show . _rawvert

instance Show Particle where
  show = show . _rawpart

instance HasLorentzVector Vertex where
  toXYZT = rawvert . rvertXYZT

instance HasLorentzVector Particle where
  toXYZT = rawpart . rpartXYZT

instance HasPID Particle where
  pid = rawpart . rpartPID


vertNOrphan :: Lens' Vertex Int
vertNOrphan = rawvert . rvertNOrphan

vertNOutgoing :: Lens' Vertex Int
vertNOutgoing = rawvert . rvertNOutgoing

vertWeights :: Lens' Vertex (Vector Double)
vertWeights = rawvert . rvertWeights


partM :: Lens' Particle Double
partM = rawpart . rpartM

partStatus :: Lens' Particle Int
partStatus = rawpart . rpartStatus

partPolarizationTheta :: Lens' Particle Double
partPolarizationTheta = rawpart . rpartPolarizationTheta

partPolarizationPhi :: Lens' Particle Double
partPolarizationPhi = rawpart . rpartPolarizationPhi

partFlows :: Lens' Particle (Vector (Int, Int))
partFlows = rawpart . rpartFlows


final :: Particle -> Bool
final = null . toListOf children

fromHadron :: Particle -> Bool
fromHadron = not . null . toListOf (ancestors . filtered unstable)
    where unstable p = view partStatus p == 2 && isHadron p

gRelatives :: Getter EventGraph G.Graph -> Fold Particle Particle
gRelatives f = folding $
    -- TODO
    -- this is inherently inefficient because it looks up vertices
    -- unnecessarily
    \p ->
      let eg = view pgraph p
          g = view f eg
          im = view rawparts eg
          i = view (rawpart.bc) p
      -- tail of reachables since the current node is always the
      -- first one listed
      in
        mapMaybe
          (fmap (Particle eg) . flip IM.lookup im)
          (tail $ G.reachable g i)

gNearestRelatives :: Getter EventGraph G.Graph -> Fold Particle Particle
gNearestRelatives f = folding $
    \p ->
      let eg = view pgraph p
          g' = view f eg
          imp = view rawparts eg
          vs = g' ! view (rawpart.bc) p
          ps = concatMap (g' !) vs
      in map (Particle eg . (IM.!) imp) ps

parents, children, descendants, ancestors :: Fold Particle Particle
parents = gNearestRelatives evtgraph'
children = gNearestRelatives evtgraph
ancestors = gRelatives evtgraph'
descendants = gRelatives evtgraph

{-# LANGUAGE TemplateHaskell #-}

module Data.HepMC.Vertex ( Vertex(..), vgraph, vgraph'
                         , vertID, vertNOrphan
                         , vertNOutgoing, vertWeights
                         , Particle(..), pgraph, pgraph'
                         , partM, partStatus
                         , partPolarizationTheta, partPolarizationPhi
                         , partFlows
                         ) where

import Control.Lens

import qualified Data.Graph as G

import Data.HepMC.Internal

import Data.HEP.PID
import Data.HEP.LorentzVector
import Data.HepMC.Barcoded


data Vertex =
    Vertex
        { _vgraph :: G.Graph 
        , _vgraph' :: G.Graph
        , _rvert :: RawVertex
        }

data Particle =
    Particle
        { _pgraph :: G.Graph 
        , _pgraph' :: G.Graph
        , _rpart :: RawParticle
        }

makeLenses ''Vertex
makeLenses ''Particle

instance Show Vertex where
    show = views rvert show

instance Show Particle where
    show = views rpart show

vertID :: Lens' Vertex Int
vertID = rvert . rvertID

vertNOrphan :: Lens' Vertex Int
vertNOrphan = rvert . rvertNOrphan

vertNOutgoing :: Lens' Vertex Int
vertNOutgoing = rvert . rvertNOutgoing

vertWeights :: Lens' Vertex [Double]
vertWeights = rvert . rvertWeights


partM :: Lens' Particle Double
partM = rpart . rpartM

partStatus :: Lens' Particle Int
partStatus = rpart . rpartStatus

partPolarizationTheta :: Lens' Particle Double
partPolarizationTheta = rpart . rpartPolarizationTheta

partPolarizationPhi :: Lens' Particle Double
partPolarizationPhi = rpart . rpartPolarizationPhi

partFlows :: Lens' Particle [(Int, Int)]
partFlows = rpart . rpartFlows


instance Barcoded Vertex where
    bc = rvert . rvertBC

instance Eq Vertex where
    (==) = liftBC2 (==)

instance Ord Vertex where
    compare = liftBC2 compare

instance HasLorentzVector Vertex where
    toXYZT = rvert . rvertXYZT

instance Barcoded Particle where
    bc = rpart . rpartBC

instance Eq Particle where
    (==) = liftBC2 (==)

instance Ord Particle where
    compare = liftBC2 compare

instance HasLorentzVector Particle where
    toXYZT = rpart . rpartXYZT

instance HasPID Particle where
    pid = rpart . rpartPID

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module HepMC.Event
    ( module X
    , Event(..)
    , parserEvent
    , Vertex
    , vertNOrphan
    , vertNOutgoing, vertWeights
    , Particle
    , partM, partStatus
    , partPolarizationTheta, partPolarizationPhi
    , partFlows
    ) where

import           Control.Lens
import           Data.Attoparsec.ByteString.Char8 as X hiding (parse)
import           Data.HEP.LorentzVector           as X
import           Data.HEP.PID
import qualified Data.IntMap                      as IM
import           HepMC.EventGraph
import           HepMC.EventHeader                as X
import           HepMC.Internal
import           HepMC.Parse



data Vertex =
  Vertex
    { _vgraph :: EventGraph
    , _rvert  :: RawVertex
    }


data Particle =
  Particle
    { _pgraph :: EventGraph
    , _rpart  :: RawParticle
    }

makeLenses ''Vertex
makeLenses ''Particle

instance Show Vertex where
  show = show . _rvert

instance Show Particle where
  show = show . _rpart

instance HasLorentzVector Vertex where
  toXYZT = rvert . rvertXYZT

instance HasLorentzVector Particle where
  toXYZT = rpart . rpartXYZT

instance HasPID Particle where
  pid = rpart . rpartPID


data Event =
  Event
    { particles :: [Particle]
    , vertices  :: [Vertex]
    , header    :: EventHeader
    }


vertNOrphan :: Lens' Vertex Int
vertNOrphan = rvert . rvertNOrphan

vertNOutgoing :: Lens' Vertex Int
vertNOutgoing = rvert . rvertNOutgoing

vertWeights :: Lens' Vertex (Vector Double)
vertWeights = rvert . rvertWeights


partM :: Lens' Particle Double
partM = rpart . rpartM

partStatus :: Lens' Particle Int
partStatus = rpart . rpartStatus

partPolarizationTheta :: Lens' Particle Double
partPolarizationTheta = rpart . rpartPolarizationTheta

partPolarizationPhi :: Lens' Particle Double
partPolarizationPhi = rpart . rpartPolarizationPhi

partFlows :: Lens' Particle (Vector (Int, Int))
partFlows = rpart . rpartFlows



parserEvent :: Parser Event
parserEvent = do
  eh <- parserEventHeader
  eg <- parserEventGraph
  let ps = Particle eg <$> IM.elems (view rawparts eg)
      vs = Vertex eg <$> IM.elems (view rawverts eg)

  return $ Event ps vs eh

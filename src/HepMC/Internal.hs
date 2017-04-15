{-# LANGUAGE TemplateHaskell #-}

module HepMC.Internal where

import           Control.Lens
import           Data.HEP.LorentzVector
import           Data.HEP.PID
import           Data.Vector
import           HepMC.Barcoded

data RawVertex =
  RawVertex
    { _rvertBC        :: Int
    , _rvertID        :: Int
    , _rvertXYZT      :: XYZT
    , _rvertNOrphan   :: Int
    , _rvertNOutgoing :: Int
    , _rvertWeights   :: Vector Double
    } deriving Show


data RawParticle =
  RawParticle
    { _rpartBC                :: Int
    , _rpartPID               :: PID
    , _rpartXYZT              :: XYZT
    , _rpartM                 :: Double
    , _rpartStatus            :: Int
    , _rpartPolarizationTheta :: Double
    , _rpartPolarizationPhi   :: Double
    , _rpartFlows             :: Vector (Int, Int)
    } deriving Show

makeLenses ''RawVertex
makeLenses ''RawParticle


instance Barcoded RawVertex where
  bc = rvertBC

instance Barcoded RawParticle where
  bc = rpartBC

instance Eq RawVertex where
  (==) = liftBC2 (==)

instance Eq RawParticle where
  (==) = liftBC2 (==)

instance Ord RawVertex where
  compare = liftBC2 compare

instance Ord RawParticle where
  compare = liftBC2 compare

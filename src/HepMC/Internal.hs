{-# LANGUAGE TemplateHaskell #-}

module HepMC.Internal where

import           Control.Lens

import           Data.HEP.LorentzVector
import           Data.HEP.PID

data RawVertex =
  RawVertex
    { _rvertBC        :: Int
    , _rvertID        :: Int
    , _rvertXYZT      :: XYZT
    , _rvertNOrphan   :: Int
    , _rvertNOutgoing :: Int
    , _rvertWeights   :: [Double]
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
    , _rpartFlows             :: [(Int, Int)]
    } deriving Show


makeLenses ''RawVertex
makeLenses ''RawParticle

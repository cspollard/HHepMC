{-# LANGUAGE TemplateHaskell #-}

module Data.HepMC.Vertex where

import Control.Lens

import Data.Vector (Vector)
import Data.Set (Set)

import Data.HEP.PID
import Data.HEP.LorentzVector
import Data.HepMC.Barcoded

data Vertex =
    Vertex
        { _vertBC :: Int
        , _vertID :: Int
        , _vertXYZT :: XYZT
        , _vertNOrphan :: Int
        , _vertNOutgoing :: Int
        , _vertWeights :: Vector Double
        , _vertParentParts :: Particles
        , _vertChildParts :: Particles
        } deriving Show


data Particle =
    Particle
        { _partBC :: Int
        , _partPID :: PID
        , _partXYZT :: XYZT
        , _partM :: Double
        , _partStatus :: Int
        , _partPolarizationTheta :: Double
        , _partPolarizationPhi :: Double
        , _partFlows :: Vector (Int, Int)
        , _partParentVert :: Vertex
        , _partChildVert :: Maybe Vertex
        } deriving Show


type Vertices = Set Vertex
type Particles = Set Particle


makeLenses ''Vertex
makeLenses ''Particle

class HasVertices hv where
    vertices :: Lens' hv Vertices

instance Barcoded Vertex where
    bc = vertBC

instance Eq Vertex where
    (==) = liftBC2 (==)

instance Ord Vertex where
    compare = liftBC2 compare

instance HasLorentzVector Vertex where
    toXYZT = vertXYZT

instance Barcoded Particle where
    bc = partBC

instance Eq Particle where
    (==) = liftBC2 (==)

instance Ord Particle where
    compare = liftBC2 compare

instance HasLorentzVector Particle where
    toXYZT = partXYZT

instance HasPID Particle where
    pid = partPID

class HasParticles hp where
    particles :: hp -> Particles

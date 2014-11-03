module Data.HepMC.EventTree where

import Data.HepMC.Vertex
import Data.HepMC.Particle

import qualified Data.IntMap as IM
import qualified Data.Tree as T

data ParticleTree =
    UnstableParticle Particle Vertex [EventTree]
    | StableParticle Particle
    deriving (Eq, Ord, Read, Show)

vertToPartTree :: Vertex -> Particles -> ParticleTree
vertToPartTree v ps = 

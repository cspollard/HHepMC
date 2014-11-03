module Data.HepMC.Units where

data UnitMomentum = MEV | GEV deriving (Eq, Ord, Read, Show)
data UnitLength = MM | CM deriving (Eq, Ord, Read, Show)

data Units = Units {
    unitEnergy :: UnitMomentum,
    unitLength :: UnitLength
} deriving (Eq, Ord, Read, Show)

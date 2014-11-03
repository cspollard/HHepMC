module Data.HepMC.Units where

data UnitMomentum = MEV | GEV deriving (Eq, Ord, Read, Show)
data UnitLength = MM | CM deriving (Eq, Ord, Read, Show)

data Units = Units {
    unitEnergy :: UnitMomentum,
    unitLength :: UnitLength
} deriving (Eq, Ord, Read, Show)


parseUnits :: Parser Units
parseUnits = do
    ue <- takeTill isSpace
    skipSpace
    ul <- takeText

    return $
        Units (read $ TS.unpack ue) (read $ TS.unpack ul)

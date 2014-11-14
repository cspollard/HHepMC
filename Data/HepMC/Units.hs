{-# LANGUAGE OverloadedStrings #-}

module Data.HepMC.Units where

import Data.HepMC.Parse

data UnitMomentum = MEV | GEV deriving (Eq, Ord, Read, Show)
data UnitLength = MM | CM deriving (Eq, Ord, Read, Show)

data Units = Units {
    unitMomentum :: UnitMomentum,
    unitLength :: UnitLength
} deriving (Eq, Ord, Read, Show)


unitP :: Parser UnitMomentum
unitP = string "MEV" *> return MEV <|>
            string "GEV" *> return GEV

unitL :: Parser UnitLength
unitL = string "MM" *> return MM <|>
            string "CM" *> return CM

parserUnits :: Parser Units
parserUnits = Units <$> (unitP <* skipSpace) <*> (unitL <* skipSpace)

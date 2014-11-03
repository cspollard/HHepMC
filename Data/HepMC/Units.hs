{-# LANGUAGE OverloadedStrings #-}

module Data.HepMC.Units where

import Data.HepMC.Parser.Common

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

parseUnits :: Parser Units
parseUnits = Units <$> unitP <*> unitL

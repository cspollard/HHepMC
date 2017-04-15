{-# LANGUAGE OverloadedStrings #-}

module HepMC.Units
  ( Units, unitMomentum, unitLength
  , parserUnits
  ) where

import           Control.Lens
import           HepMC.Parse

data UnitMomentum = MEV | GEV deriving (Eq, Ord, Show)
data UnitLength = MM | CM deriving (Eq, Ord, Show)

type Units = (UnitMomentum, UnitLength)

unitMomentum :: Lens' Units UnitMomentum
unitMomentum = _1

unitLength :: Lens' Units UnitLength
unitLength = _2

unitP :: Parser UnitMomentum
unitP =
  string "MEV" *> return MEV <|> string "GEV" *> return GEV

unitL :: Parser UnitLength
unitL =
  string "MM" *> return MM <|> string "CM" *> return CM

parserUnits :: Parser Units
parserUnits = tuple (unitP <* skipSpace) (unitL <* skipSpace)

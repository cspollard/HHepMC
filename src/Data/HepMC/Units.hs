{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.HepMC.Units where

import Control.Lens

import Data.HepMC.Parse

data UnitMomentum = MEV | GEV deriving (Eq, Ord, Show)
data UnitLength = MM | CM deriving (Eq, Ord, Show)

data Units =
    Units
        { _unitMomentum :: UnitMomentum
        , _unitLength :: UnitLength
        } deriving Show

makeLenses ''Units


unitP :: Parser UnitMomentum
unitP = string "MEV" *> return MEV <|>
            string "GEV" *> return GEV

unitL :: Parser UnitLength
unitL = string "MM" *> return MM <|>
            string "CM" *> return CM

parserUnits :: Parser Units
parserUnits = Units <$> (unitP <* skipSpace) <*> (unitL <* skipSpace)

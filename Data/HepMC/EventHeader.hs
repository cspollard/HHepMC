module Data.HepMC.EventHeader where

import Data.HepMC.EventInfo
import Data.HepMC.PDFInfo
import Data.HepMC.Units
import Data.HepMC.HeavyIonInfo
import qualified Data.Text.Lazy as TL

type WeightNames = [TL.Text]

type CrossSection = (Double, Double)

data EventHeader = EventHeader {
    eventInfo :: EventInfo,
    weightNames :: Maybe WeightNames,
    units :: Units,
    crossSection :: Maybe CrossSection,
    heavyIonInfo :: Maybe HeavyIonInfo,
    pdfInfo :: Maybe PDFInfo
} deriving (Eq, Ord, Read, Show)


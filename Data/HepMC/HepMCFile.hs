module Data.HepMC.HepMCFile where

import qualified Data.Text.Lazy as TL
import Data.HepMC.Event

type Version = TL.Text

data HepMCFile = HepMCFile {
    version :: Version,
    events :: [Event]
} deriving (Eq, Ord, Read, Show)

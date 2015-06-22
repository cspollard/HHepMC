module Data.HepMC.Event where

import Data.HepMC.Parse
import Data.HepMC.EventHeader
import Data.HepMC.HepMCVertex
import Data.HepMC.HepMCParticle
import Data.HepMC.Barcoded
import Data.HepMC.Vertex
import Data.HepMC.EventGraph
import Data.Either
import Data.ABGraph
import Data.List (sortBy, sort)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM


data Event = Event {
    evtHeader :: EventHeader,
    evtGraph :: EventGraph
}

parserEvent :: Parser Event
parserEvent = Event <$> parserEventHeader <*> parserEventGraph

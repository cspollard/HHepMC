module Data.HepMC.Event where

import Data.HepMC.Parse
import Data.HepMC.EventHeader
import Data.HepMC.EventGraph

data Event = Event {
    evtHeader :: EventHeader,
    evtGraph :: EventGraph
}

parserEvent :: Parser Event
parserEvent = Event <$> parserEventHeader <*> parserEventGraph

module Data.Jet where

import Data.Jet.ClusterStrategy
import Data.Jet.PseudoJet


cluster :: ClusterStrategy -> [PseudoJet] -> [PseudoJet]

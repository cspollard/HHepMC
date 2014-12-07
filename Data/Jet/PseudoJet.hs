module Data.Jet.PseudoJet where

import Data.HepMC.FourMomentum
import Data.HepMC.XYZT
import Data.HepMC.Barcoded

data PseudoJet = PseudoJet BC XYZT (Maybe (PseudoJet, PseudoJet))

instance Barcoded PseudoJet where
    bc (PseudoJet b _ _) = b

instance Eq PseudoJet where
    (==) = liftBC2 (==)

instance Ord PseudoJet where
    compare = liftBC2 compare


instance FourMomentum PseudoJet where
    xV (PseudoJet v _) = xV v
    yV (PseudoJet v _) = yV v
    zV (PseudoJet v _) = zV v
    tV (PseudoJet v _) = tV v

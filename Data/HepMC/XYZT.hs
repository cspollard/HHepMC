module Data.HepMC.XYZT where

import Data.HepMC.Parse
import Data.HepMC.FourMomentum

data XYZT = XYZT Double Double Double Double
    deriving (Eq, Ord, Read, Show)

instance FourMomentum XYZT where
    xV (XYZT x _ _ _) = x
    yV (XYZT _ y _ _) = y
    zV (XYZT _ _ z _) = z
    tV (XYZT _ _ _ t) = t

toXYZT :: FourMomentum a => a -> XYZT
toXYZT v = XYZT (xV v) (yV v) (zV v) (tV v)

parserXYZT :: Parser XYZT
parserXYZT = XYZT <$> doubSpace <*> doubSpace <*> doubSpace <*> double

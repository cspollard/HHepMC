module XYZT where

import Data.HepMC.Parser.Common
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

parseXYZT :: Parser XYZT
parseXYZT = XYZT <$> doubSpace <*> doubSpace <*> doubSpace <*> double

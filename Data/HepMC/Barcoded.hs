module Data.HepMC.Barcoded where

type BC = Int

class Barcoded b where
    bc :: b -> BC


instance Eq a => Eq ((,) a a) where
   (w, x) == (y, z) = w == y && x == z

instance Barcoded b => Eq b where
    (==) = liftBC2 (==)

instance (Barcoded b) => Ord b where
    compare = liftBC2 compare


liftBC :: Barcoded a => (BC -> b) -> a -> b
liftBC f = f . bc

liftBC2 :: (Barcoded a, Barcoded b) => (BC -> BC -> c) -> a -> b -> c
liftBC2 f a b = f (bc a) (bc b)

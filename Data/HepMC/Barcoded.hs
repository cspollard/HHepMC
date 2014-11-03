module Data.HepMC.Barcoded where

type BC = Int

class Barcoded b where
    bc :: b -> BC

liftBC :: Barcoded a => (BC -> b) -> a -> b
liftBC f = f . bc

liftBC2 :: (Barcoded a, Barcoded b) => (BC -> BC -> c) -> a -> b -> c
liftBC2 f a b = f (bc a) (bc b)

module Data.HepMC.Barcode where

type BC = Int

class Barcode b where
    bc :: b -> BC

liftBC :: Barcode a => (BC -> b) -> a -> b
liftBC f = f . bc

liftBC2 :: (Barcode a, Barcode b) => (BC -> BC -> c) -> a -> b -> c
liftBC2 f a b = f (bc a) (bc b)

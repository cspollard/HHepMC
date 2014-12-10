module Data.HepMC.Barcoded where

import qualified Data.IntMap as IM
import Data.IntMap (IntMap)

type BC = Int

class Barcoded b where
    bc :: b -> BC


liftBC :: Barcoded a => (BC -> b) -> a -> b
liftBC f = f . bc

liftBC2 :: (Barcoded a, Barcoded b) => (BC -> BC -> c) -> a -> b -> c
liftBC2 f a b = f (bc a) (bc b)


withBC :: Barcoded b => b -> (BC, b)
withBC b = (bc b, b)

insertBC :: Barcoded b => b -> IntMap b -> IntMap b
insertBC b = IM.insert (bc b) b

deleteBC :: Barcoded b => b -> IntMap b -> IntMap b
deleteBC b = IM.delete (bc b)

fromListBC :: Barcoded b => [b] -> IntMap b
fromListBC = IM.fromList . map withBC

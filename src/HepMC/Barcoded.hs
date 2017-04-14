module HepMC.Barcoded where

import Control.Lens

import qualified Data.IntMap as IM
import Data.IntMap (IntMap)

type BC = Int

class Barcoded b where
    bc :: Lens' b BC

instance (Barcoded a, Barcoded b) => Barcoded (Either a b) where
    bc = choosing bc bc


liftBC :: Barcoded a => (BC -> b) -> a -> b
liftBC f = f . view bc

liftBC2 :: (Barcoded a, Barcoded b) => (BC -> BC -> c) -> a -> b -> c
liftBC2 f a b = f (view bc a) (view bc b)


withBC :: Barcoded b => b -> (BC, b)
withBC b = (view bc b, b)

insertBC :: Barcoded b => b -> IntMap b -> IntMap b
insertBC b = IM.insert (view bc b) b

deleteBC :: Barcoded b => b -> IntMap b -> IntMap b
deleteBC = sans . view bc

fromListBC :: Barcoded b => [b] -> IntMap b
fromListBC = IM.fromList . map withBC

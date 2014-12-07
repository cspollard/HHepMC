module Data.HJet.ClusterStrategy where

import Data.HepMC.FourMomentum

class ClusterStrategy c where
    dij :: (FourMomentum a, FourMomentum b) => c -> a -> b -> Double
    diB :: FourMomentum a => c -> a -> Double


data KTLike = KTLike { r0 :: Double, p :: Int }

antiKt :: Double -> KTLike
kt :: Double -> KTLike
camKt :: Double -> KTLike

antiKt = flip KTLike (-1)
kt = flip KTLike 1
camKt = flip KTLike 0


instance ClusterStrategy KTLike where
    dij c i j = ((ptV i `min` ptV j) ^ p c) * dRV i j / r0 c
    diB c i = ptV i ^ p c

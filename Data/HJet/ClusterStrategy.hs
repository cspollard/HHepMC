module Data.HJet.ClusterStrategy where

import Data.LorentzVector

class ClusterStrategy c where
    dij :: (LorentzVector a, LorentzVector b) => c -> a -> b -> Double
    diB :: LorentzVector a => c -> a -> Double


data KTLike = KTLike { r0 :: Double, p :: Int }

antiKt :: Double -> KTLike
kt :: Double -> KTLike
camKt :: Double -> KTLike

antiKt = flip KTLike (-1)
kt = flip KTLike 1
camKt = flip KTLike 0

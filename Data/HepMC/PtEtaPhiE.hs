module XYZT where

import Data.HepMC.Parser.Common
import Data.HepMC.FourMomentum

data PtEtaPhiE = PtEtaPhiE Double Double Double Double
    deriving (Eq, Ord, Read, Show)

instance FourMomentum PtEtaPhiE where
    ptV (PtEtaPhiE pt _ _ _) = pt
    etaV (PtEtaPhiE _ eta _ _) = eta
    phiV (PtEtaPhiE _ _ phi _) = phi
    eV (PtEtaPhiE _ _ _ e) = e

toPtEtaPhiE :: FourMomentum a => a -> PtEtaPhiE
toPtEtaPhiE v = PtEtaPhiE (ptV v) (etaV v) (phiV v) (eV v)

parsePtEtaPhiE :: Parser PtEtaPhiE
parsePtEtaPhiE = PtEtaPhiE <$> doubSpace <*> doubSpace <*> doubSpace <*> double

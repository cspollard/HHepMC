module Data.HepMC.LorentzVector where

sq :: Num a => a -> a
sq x = x*x

sqrt' :: (Ord a, Floating a) => a -> a
sqrt' x = if x < 0 then (- sqrt x) else sqrt x

-- minimum definition: xV, yV, zV, tV
class Eq v => LorentzVector v where
    xV :: v -> Double
    xV = pxV

    yV :: v -> Double
    yV = pyV

    zV :: v -> Double
    zV = pzV

    tV :: v -> Double
    tV = eV

    pxV :: v -> Double
    pxV w = ptV w * cos (phiV w)

    pyV :: v -> Double
    pyV w = ptV w * sin (phiV w)

    pzV :: v -> Double
    pzV w = pV w * sin (thetaV w)

    eV :: v -> Double
    eV w = sqrt (p2V w + m2V w)

    pt2V :: v -> Double
    pt2V w = sq (pxV w) + sq (pyV w)

    p2V :: v -> Double
    p2V w = pt2V w + sq (pzV w)

    m2V :: v -> Double
    m2V w = sq (tV w) - p2V w

    ptV :: v -> Double
    ptV = sqrt . pt2V

    pV :: v -> Double
    pV = sqrt . p2V

    mV :: v -> Double
    mV = sqrt' . m2V

    etaV :: v -> Double
    etaV w = 0.5 * log ((p + z) / (p - z))
        where
            p = pV w
            z = zV w

    thetaV :: v -> Double
    thetaV w = atan2 (ptV w) (pzV w)

    phiV :: v -> Double
    phiV w = atan2 (yV w) (xV w)

    dRV :: v -> v -> Double
    dRV x w = sqrt(sq deta + sq dphi)
        where
            deta = etaV x - etaV w
            dphi = phiV x - phiV w


data XYZT = XYZT Double Double Double Double
    deriving (Eq, Ord, Read, Show)

data PtEtaPhiE = PtEtaPhiE Double Double Double Double
    deriving (Eq, Ord, Read, Show)

instance LorentzVector XYZT where
    xV (XYZT x _ _ _) = x
    yV (XYZT _ y _ _) = y
    zV (XYZT _ _ z _) = z
    tV (XYZT _ _ _ t) = t

instance LorentzVector PtEtaPhiE where
    ptV (PtEtaPhiE pt _ _ _) = pt
    etaV (PtEtaPhiE _ eta _ _) = eta
    phiV (PtEtaPhiE _ _ phi _) = phi
    eV (PtEtaPhiE _ _ _ e) = e

module Data.HepMC.FourMomentum where

sq :: Num a => a -> a
sq x = x*x

-- minimum definition: xV, yV, zV, tV
class FourMomentum v where
    xV :: v -> Double
    xV w = ptV w * cos (phiV w)

    yV :: v -> Double
    yV w = ptV w * sin (phiV w)

    zV :: v -> Double
    zV w = pV w * sin (thetaV w)

    tV :: v -> Double
    tV w = sqrt (p2V w + m2V w)

    pxV :: v -> Double
    pxV = xV

    pyV :: v -> Double
    pyV = yV

    pzV :: v -> Double
    pzV = zV

    eV :: v -> Double
    eV = tV

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
    mV = sqrt . m2V

    etaV :: v -> Double
    etaV w = 0.5 * log ((p + z) / (p - z))
        where
            p = pV w
            z = zV w

    thetaV :: v -> Double
    thetaV w = atan2 (ptV w) (pzV w)

    phiV :: v -> Double
    phiV w = atan2 (yV w) (xV w)

    dRV :: FourMomentum y => v -> y -> Double
    dRV x w = sqrt(sq deta + sq dphi)
        where
            deta = etaV x - etaV w
            dphi = phiV x - phiV w

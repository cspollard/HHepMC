{-
 - taken from
 - http://pdg.lbl.gov/2002/montecarlorpp.pdf
 -}

module Data.HepMC.PID where

import Data.Set

-- TODO
-- Integer?
type PID = Int

type PIDSet = Set PID

class HasPID hp where
    pid :: hp -> PID

abspid :: HasPID hp => hp -> PID
abspid = abs . pid


electron, eNeutrino, muon, mNeutrino, tau, tNeutrino :: Int

down, up, strange, charm, bottom, top :: Int

gluon, photon, gamma, z, wplus, wminus :: Int

h, hplus, hminus :: Int


electron = 11
eNeutrino = 12

muon = 13
mNeutrino = 14

tau = 15
tNeutrino = 16

down = 1
up = 2
strange = 3
charm = 4
bottom = 5
top = 6

gluon = 21
photon = 22
gamma = photon
z = 23
wplus = 24
wminus = -wplus

-- TODO
-- BSM higgses...
-- a0?
-- h0?

h = 25

hplus = 37
hminus = -hplus

chargedLeptons = fromList [electron, -electron,
                           muon, -muon,
                           tau, -tau]

neutrinos = fromList [eNeutrino, -eNeutrino,
                      mNeutrino, -mNeutrino,
                      tNeutrino, -tNeutrino]

leptons = union chargedLeptons neutrinos



downTypeQuarks = fromList [down, -down,
                           strange, -strange,
                           bottom, -bottom]

upTypeQuarks = fromList [up, -up,
                         charm, -charm,
                         top, -top]


quarks = union downTypeQuarks upTypeQuarks

partons = insert gluon quarks

weakBosons = fromList [z, wplus, wminus]

ewBosons = insert photon weakBosons


diquark p = 1000 < p && p < 7000
hadron p = nq2 p > 0 && nq2 p > 0
meson p = hadron p && nq1 p == 0
baryon p = hadron p && nq1 p > 0

digit :: (Integral a) => a -> a -> a
digit x n = div (abs x) (10^n) `mod` 10

nJ = flip digit 0
nq3 = flip digit 1
nq2 = flip digit 2
nq1 = flip digit 3
nL = flip digit 4
nr = flip digit 5
n = flip digit 6

hasQuark p q = or . fmap ( (==) q . ($ p) ) $ [nq1, nq2, nq3]

hasBottomQuark = hasQuark bottom
hasCharmQuark = hasQuark charm

ofType :: HasPID hp => hp -> PIDSet -> Bool
ofType p = member (pid p)

-- TODO
-- K long and K short

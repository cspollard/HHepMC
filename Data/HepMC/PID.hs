module Data.HepMC.PID where

import Data.Set

type PID = Int

class HasPID hp where
    pid :: hp -> PID

    abspid :: hp -> PID
    abspid = abs . pid

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

isNeutrino = member neutrinos
isChargedLepton = member chargedLeptons
isLepton = member leptons


downTypeQuarks = fromList [down, -down,
                           strange, -strange,
                           bottom, -bottom]

upTypeQuarks = fromList [up, -up,
                         charm, -charm,
                         top, -top]


quarks = union downTypeQuarks upTypeQuarks

isDownTypeQUark = member downTypeQuarks
isUpTypeQUark = member upTypeQuarks
isQuark = member quarks

partons = insert gluon quarks

isParton = member partons

weakBosons = fromList [z, wplus, wminus]

ewBosons = insert photon weakBosons

module Data.HepMC.PID where

type PID = Int

class HasPID hp where
    pid :: hp -> PID

    abspid :: hp -> PID
    abspid = abs . pid

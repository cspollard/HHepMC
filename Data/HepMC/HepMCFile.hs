{-# LANGUAGE OverloadedStrings #-}

module Data.HepMC.HepMCFile where

import Data.Text.Lazy (Text, fromStrict)
import Data.HepMC.Parser.Common
import Data.HepMC.Event
import Data.Queue

type Version = Text

data HepMCFile = HepMCFile {
    version :: Version,
    events :: Queue Event
} deriving Show


parserVersion :: Parser Version
parserVersion = do
    skipSpace
    _ <- string "HepMC::Version"
    skipSpace
    v <- takeTill isEndOfLine <* endOfLine
    _ <- string "HepMC::IO_GenEvent-START_EVENT_LISTING"
    skipSpace
    return $ fromStrict v


parserHepMC :: Parser HepMCFile
parserHepMC = HepMCFile <$> parserVersion <*> manyQ parserEvent

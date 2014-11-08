{-# LANGUAGE OverloadedStrings #-}

module Data.HepMC.HepMCFile where

import Data.Text.Lazy (Text, fromStrict)
import Data.HepMC.Parser.Common
import Data.HepMC.Event

type Version = Text

data HepMCFile = HepMCFile {
    version :: Version,
    events :: [Event]
} deriving Show


parserVersion :: Parser Version
parserVersion = do
    _ <- string "HepMC::Version"
    skipSpace
    v <- takeTill isEndOfLine <* endOfLine
    _ <- string "HepMC::IO_GenEvent-START_EVENT_LISTING"
    skipSpace
    return $ fromStrict v


-- parserHepMC :: Parser HepMCFile
-- parserHepMC = HepMCFile <$> parserVersion <*> parserEvent

{-# LANGUAGE OverloadedStrings #-}

module Data.HepMC.File where

import Data.Text.Lazy (Text, fromStrict)
import Data.HepMC.Parse
import Data.HepMC.Event

type Version = Text

data File = File {
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


-- parserHepMC :: Parser File
-- parserHepMC = File <$> parserVersion <*> parserEvent

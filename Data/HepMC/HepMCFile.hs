module Data.HepMC.HepMCFile where

import qualified Data.Text.Lazy as TL
import Data.HepMC.Event
import Data.Attoparsec.Text.Lazy
import Control.Applicative


type Version = TL.Text

data HepMCFile = HepMCFile {
    version :: Version,
    events :: [Event]
} deriving (Eq, Ord, Read, Show)


parseHepMCHeader :: Parser Version
parseHepMCHeader = do
    skipSpace
    string "HepMC::Version"
    skipSpace
    v <- manyTill endOfLine
    skipSpace
    string "HepMC::IO_GenEvent-START_EVENT_LISTING"
    skipSpace
    return v


parseEvents :: TL.Text -> [Event]
parseEvents "" = []
parseEvents t = case r of
                    Done t' e -> e
    where
        r = 


hepMCFile :: TL.Text -> Maybe HepMCFile
hepMCFile t = HepMCFile <$> v' <*> es
    where
        v = parse parseHepMCHeader

    HepMCFile  many parseEvent

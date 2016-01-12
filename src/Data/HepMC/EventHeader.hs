module Data.HepMC.EventHeader where

import Data.Text.Lazy (Text, fromStrict)
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Data.HepMC.Parse
import Data.HepMC.EventInfo
import Data.HepMC.PDFInfo
import Data.HepMC.Units
import Data.HepMC.HeavyIonInfo

import Debug.Trace


type WeightNames = [Text]

parserWeightNames :: Parser WeightNames
parserWeightNames = do
    n <- decimal <* skipSpace
    count n $ quote <* skipSpace


type CrossSection = (Double, Double)

parserCrossSection :: Parser CrossSection
parserCrossSection = do
    char 'C' >> skipSpace
    tuple (double <* skipSpace) (double <* skipSpace)



parseHeaderLine :: Parser (Char, Text)
parseHeaderLine = (,) <$> satisfy (inClass "ENUCHF") <* skipSpace <*> (fromStrict <$> takeTill isEndOfLine <* endOfLine)


{-
parserEventHeader :: Parser EventHeader
parserEventHeader = let m = M.fromList <$> many parseHeaderLine in
    EventHeader <$> parseEventInfo <*>

    let wn = parseWeightNames M.lookup 'N' m
    let u = fromJust $ maybeResult . parse parserUnits =<< M.lookup 'U' m
    let cs = maybeResult . parse parserCrossSection =<< M.lookup 'C' m
    let hii = maybeResult . parse parserHeavyIonInfo =<< M.lookup 'H' m
    let pdfi = maybeResult . parse parserPDFInfo =<< M.lookup 'F' m

    return $ EventHeader ei wn u cs hii pdfi
    -}

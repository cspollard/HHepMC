module Data.HepMC.EventHeader where

import Data.Text.Lazy (Text, fromStrict)
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Data.HepMC.Parse
import Data.HepMC.EventInfo
import Data.HepMC.PDFInfo
import Data.HepMC.Units
import Data.HepMC.HeavyIonInfo


type WeightNames = [Text]

parserWeightNames :: Parser WeightNames
parserWeightNames = do
    n <- decSpace
    count n $ quote <* skipSpace


type CrossSection = (Double, Double)

parserCrossSection :: Parser CrossSection
parserCrossSection = tuple doubSpace doubSpace


data EventHeader = EventHeader {
    eventInfo :: EventInfo,
    weightNames :: Maybe WeightNames,
    units :: Units,
    crossSection :: Maybe CrossSection,
    heavyIonInfo :: Maybe HeavyIonInfo,
    pdfInfo :: Maybe PDFInfo
} deriving (Eq, Ord, Read, Show)


parserHeaderLine :: Parser (Char, Text)
parserHeaderLine = do
    k <- satisfy $ inClass "NUCHF"; skipSpace
    r <- takeTill isEndOfLine <* endOfLine

    return (k, fromStrict r)


parserEventHeader :: Parser EventHeader
parserEventHeader = do
    ei <- parserEventInfo
    m <- M.fromList `fmap` many parserHeaderLine

    let wn = maybeResult . parse parserWeightNames =<< M.lookup 'N' m
    let u = fromJust $ maybeResult . parse parserUnits =<< M.lookup 'U' m
    let cs = maybeResult . parse parserCrossSection =<< M.lookup 'C' m
    let hii = maybeResult . parse parserHeavyIonInfo =<< M.lookup 'H' m
    let pdfi = maybeResult . parse parserPDFInfo =<< M.lookup 'F' m

    return $ EventHeader ei wn u cs hii pdfi

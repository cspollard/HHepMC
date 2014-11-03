module Data.HepMC.EventHeader where

import Data.HepMC.Parser.Common
import Data.HepMC.EventInfo
import Data.HepMC.PDFInfo
import Data.HepMC.Units
import Data.HepMC.HeavyIonInfo

type WeightNames = [TL.Text]

parseWeightNames :: Parser WeightNames
parseWeightNames = do
    n <- decimal
    s <- count n quote

    return s


type CrossSection = (Double, Double)

parseCrossSection :: Parser CrossSection
parseCrossSection = tuple doubSpace doubSpace


data EventHeader = EventHeader {
    eventInfo :: EventInfo,
    weightNames :: Maybe WeightNames,
    units :: Units,
    crossSection :: Maybe CrossSection,
    heavyIonInfo :: Maybe HeavyIonInfo,
    pdfInfo :: Maybe PDFInfo
} deriving (Eq, Ord, Read, Show)


parseHeaderLine :: Parser (Char, TL.Text)
parseHeaderLine = do
    k <- satisfy $ inClass "NUCHF"; skipSpace
    r <- toEndLine

    return (k, r)


parseEventHeader :: Parser EventHeader
parseEventHeader = do
    ei <- parseEventInfo
    m <- M.fromList `fmap` many parseHeaderLine

    let wn = maybeResult . parse parseWeightNames =<< M.lookup 'N' m
    let u = fromJust $ maybeResult . parse parseUnits =<< M.lookup 'U' m
    let cs = maybeResult . parse parseCrossSection =<< M.lookup 'C' m
    let hii = maybeResult . parse parseHeavyIonInfo =<< M.lookup 'H' m
    let pdfi = maybeResult . parse parsePDFInfo =<< M.lookup 'F' m

    return $ EventHeader ei wn u cs hii pdfi


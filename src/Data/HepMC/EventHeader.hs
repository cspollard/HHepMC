module Data.HepMC.EventHeader where

import Data.ByteString
import Data.HepMC.Parse


-- type WeightNames = Vector Text

-- parserWeightNames :: Parser WeightNames
-- parserWeightNames = hepMCList


type CrossSection = (Double, Double)

parserCrossSection :: Parser CrossSection
parserCrossSection = do
    char 'C' >> skipSpace
    tuple (double <* skipSpace) (double <* skipSpace)


parseHeaderLine :: Parser (Char, ByteString)
parseHeaderLine = (,) <$> satisfy (inClass "ENUCHF") <* char ' ' <*> (takeTill isEndOfLine <* endOfLine)


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

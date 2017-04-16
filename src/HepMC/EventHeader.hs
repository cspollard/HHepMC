{-# LANGUAGE TemplateHaskell #-}

module HepMC.EventHeader
  ( module X
  , parserEventHeader
  , EventHeader
  , eventInfo, weightNames, units, crossSection, heavyIonInfo, pdfInfo
  ) where

import           Control.Lens
import           Data.Text          (Text)
import           HepMC.CrossSection as X
import           HepMC.EventInfo    as X
import           HepMC.HeavyIonInfo as X
import           HepMC.Parse
import           HepMC.PDFInfo      as X
import           HepMC.Units        as X
import           HepMC.WeightNames  as X


data EventHeader =
  EventHeader
    { _eventInfo    :: EventInfo
    , _weightNames  :: Maybe (Vector Text)
    , _units        :: Units
    , _crossSection :: Maybe CrossSectionInfo
    , _heavyIonInfo :: Maybe HeavyIonInfo
    , _pdfInfo      :: Maybe PDFInfo
    } deriving Show

makeLenses ''EventHeader

data HeaderInfo =
  C CrossSectionInfo
  | E EventInfo
  | F PDFInfo
  | H HeavyIonInfo
  | N WeightNames
  | U Units
  deriving Show

makePrisms ''HeaderInfo


evtHeaderLine :: Parser HeaderInfo
evtHeaderLine = do
  hi <- satisfy (inClass "CEFHNU") <* skipSpace
  case hi of
    'C' -> C <$> parserCrossSectionInfo
    'E' -> E <$> parserEventInfo
    'F' -> F <$> parserPDFInfo
    'H' -> H <$> parserHeavyIonInfo
    'N' -> N <$> parserWeightNames
    'U' -> U <$> parserUnits
    _   -> fail "invalid event header lead character"


parserEventHeader :: Parser EventHeader
parserEventHeader = do
  hls <- many1 evtHeaderLine
  evtinfo <-
    maybe (fail "missing event info!") return
    $ hls ^? traverse . _E
  u <-
    maybe (fail "missing units!") return
    $ hls ^? traverse . _U

  return
    $ EventHeader
      evtinfo
      (hls ^? traverse . _N)
      u
      (hls ^? traverse . _C)
      (hls ^? traverse . _H)
      (hls ^? traverse . _F)

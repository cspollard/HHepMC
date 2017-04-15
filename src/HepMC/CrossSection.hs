module HepMC.CrossSection where

import           Control.Lens
import           HepMC.Parse

type CrossSectionInfo = (Double, Double)

xsecCentral :: Field1 s t a b => Lens s t a b
xsecCentral = _1

xsecUncert :: Field2 s t a b => Lens s t a b
xsecUncert = _2


parserCrossSectionInfo :: Parser CrossSectionInfo
parserCrossSectionInfo =
  tuple (double <* skipSpace) (double <* skipSpace)

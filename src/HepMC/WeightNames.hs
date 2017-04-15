module HepMC.WeightNames where

import           Data.Text   (Text, pack)
import           HepMC.Parse

type WeightNames = Vector Text

parserWeightNames :: Parser WeightNames
parserWeightNames =
  vector (fmap pack $ many1 (satisfy $ not . isSpace) <* skipSpace)

module Data.HepMC.Parser.Common (
    module Control.Applicative,
    module Data.Functor,
    module Data.Attoparsec.Text.Lazy,
    decSpace,
    doubSpace,
    tuple,
    quote
) where

import Data.Attoparsec.Text.Lazy
import Data.Text.Lazy (Text, pack)
import Control.Applicative (Applicative(..), Alternative(..))
import Data.Functor (Functor(..), (<$>))

decSpace :: Parser Int
decSpace = signed decimal <* skipSpace

doubSpace :: Parser Double
doubSpace = signed double <* skipSpace

tuple :: Parser a -> Parser b -> Parser (a, b)
tuple p q = (,) <$> p <*> q

quote :: Parser Text
quote = pack <$> (char '"' *> manyTill anyChar (char '"'))
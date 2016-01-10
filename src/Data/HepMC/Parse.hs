module Data.HepMC.Parse (
    module Control.Applicative,
    module Data.Functor,
    module Data.Attoparsec.Text.Lazy,
    tuple,
    quote,
    hepmcList
) where

import Data.Attoparsec.Text.Lazy hiding (take)
import Data.Text.Lazy (Text, pack)
import Control.Applicative (Applicative(..), Alternative(..), liftA2)
import Data.Functor (Functor(..), (<$>))
import Control.Monad (join)
import Data.Vector

tuple :: Parser a -> Parser b -> Parser (a, b)
tuple = liftA2 (,)

quote :: Parser Text
quote = pack <$> (char '"' *> manyTill anyChar (char '"'))


-- parse a list of objects: first is the decimal length of the list
-- followed by the objects (separated by spaces)
-- hepmcList :: Parser a -> Parser (Vector a)
hepmcList :: Parser a -> Parser [a]
hepmcList p = do
    n <- decimal <* skipSpace
    count n (p <* skipSpace)
    -- replicateM n (p <* skipSpace)


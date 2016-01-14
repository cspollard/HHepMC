{-# LANGUAGE OverloadedStrings #-}

module Data.HepMC.Parse (
    module Control.Applicative,
    module Data.Functor,
    module Data.Attoparsec.ByteString.Char8,
    tuple,
    quote,
    hepmcList,
    isEndOfLine,
    parserVersion
) where

import Data.Attoparsec.ByteString.Char8 hiding (isEndOfLine)
import Data.ByteString (ByteString)
import Data.Text.Lazy (Text, pack)
import Control.Applicative (Applicative(..), Alternative(..), liftA2)
import Data.Functor (Functor(..), (<$>))
import Control.Monad (join)
import Data.Vector

parserVersion :: Parser ByteString
parserVersion = do
    _ <- string "HepMC::Version"
    skipSpace
    v <- takeTill isEndOfLine <* endOfLine
    _ <- string "HepMC::IO_GenEvent-START_EVENT_LISTING"
    skipSpace
    return v


isEndOfLine :: Char -> Bool
isEndOfLine w = w == '\r' || w == '\n'

tuple :: Parser a -> Parser b -> Parser (a, b)
tuple p q = (,) <$> p <* skipSpace <*> q

quote :: Parser Text
quote = pack <$> (char '"' *> manyTill anyChar (char '"'))


-- parse a vector of objects: first is the decimal length of the list
-- followed by the objects (separated by spaces)
hepmcList :: Parser a -> Parser (Vector a)
hepmcList p = do
    n <- decimal
    replicateM n (skipSpace *> p)

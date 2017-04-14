{-# LANGUAGE OverloadedStrings #-}

module HepMC.Parse
  ( module X
  , tuple, vector, eol
  , hmcvers, hmcend, evtHeaderLine
  , HeaderInfo(..)
  ) where

import           Control.Applicative              as X ((<|>))
import           Control.Applicative              (liftA2)
import           Data.Attoparsec.ByteString.Char8 as X hiding (parse)
import           Data.ByteString                  (ByteString)
import qualified Data.Vector.Generic              as V


tuple :: Applicative f => f a -> f b -> f (a, b)
tuple = liftA2 (,)

vector :: V.Vector v a => Parser a -> Parser (v a)
vector p = do
  n <- decimal <* skipSpace
  V.replicateM n p

eol :: Char -> Bool
eol = isEndOfLine . toEnum . fromEnum


hmcvers :: Parser (Int, Int, Int)
hmcvers = do
  skipSpace
  string "HepMC::Version" *> skipSpace
  x <- decimal <* char '.'
  y <- decimal <* char '.'
  z <- decimal <* skipSpace
  string "HepMC::IO_GenEvent-START_EVENT_LISTING" *> skipSpace
  return (x, y, z)


hmcend :: Parser ByteString
hmcend = do
  _ <- string "HepMC::IO_GenEvent-END_EVENT_LISTING"
  skipSpace
  return "end"


data HeaderInfo = C | E | F | H | N | U deriving (Eq, Ord, Show, Read)


evtHeaderLine :: Parser (HeaderInfo, ByteString)
evtHeaderLine = do
  hi <- read . pure <$> satisfy (inClass "CEFHNU")
  bs <- skipSpace *> takeTill eol <* endOfLine
  return (hi, bs)

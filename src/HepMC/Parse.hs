{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module HepMC.Parse
  ( module X
  , tuple, vector, eol
  , hmcvers, hmcend
  , xyzt
  ) where

import           Control.Applicative              as X (many, (<|>))
import           Control.Applicative              (liftA2)
import           Control.Monad                    as X (void)
import           Data.Attoparsec.ByteString.Char8 as X hiding (parse)
import           Data.ByteString                  (ByteString)
import           Data.HEP.LorentzVector
import           Data.Vector                      as X (Vector)
import           Data.Vector


tuple :: Applicative f => f a -> f b -> f (a, b)
tuple = liftA2 (,)

vector :: Parser a -> Parser (Vector a)
vector p = do
  n <- decimal <* skipSpace
  replicateM n p

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


xyzt :: Parser XYZT
xyzt =
  XYZT
    <$> double <* skipSpace
    <*> double <* skipSpace
    <*> double <* skipSpace
    <*> double

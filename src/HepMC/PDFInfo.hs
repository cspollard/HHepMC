{-# LANGUAGE TemplateHaskell #-}

module HepMC.PDFInfo where

import           Control.Lens
import           HepMC.Parse

data PDFInfo =
  PDFInfo
    { pdfID1     :: Int
    , _pdfID2    :: Int
    , _pdfX1     :: Double
    , _pdfX2     :: Double
    , _qScale    :: Double
    , _pdfXfx1   :: Double
    , _pdfXfx2   :: Double
    , _pdfSetID1 :: Int
    , _pdfSetID2 :: Int
    } deriving Show

makeLenses ''PDFInfo

parserPDFInfo :: Parser PDFInfo
parserPDFInfo =
  PDFInfo
    <$> decimal <* skipSpace
    <*> decimal <* skipSpace
    <*> double <* skipSpace
    <*> double <* skipSpace
    <*> double <* skipSpace
    <*> double <* skipSpace
    <*> double <* skipSpace
    <*> decimal <* skipSpace
    <*> decimal <* skipSpace

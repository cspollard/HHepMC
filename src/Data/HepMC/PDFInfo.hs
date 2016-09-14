{-# LANGUAGE TemplateHaskell #-}

module Data.HepMC.PDFInfo where

import Control.Lens

import Data.HepMC.Parse

data PDFInfo =
    PDFInfo
        { pdfID1 :: Int
        , _pdfID2 :: Int
        , _pdfX1 :: Double
        , _pdfX2 :: Double
        , _qScale :: Double
        , _pdfXfx1 :: Double
        , _pdfXfx2 :: Double
        , _pdfSetID1 :: Int
        , _pdfSetID2 :: Int
        } deriving (Eq, Ord, Read, Show)

makeLenses ''PDFInfo

parserPDFInfo :: Parser PDFInfo
parserPDFInfo = do
    char 'P' >> skipSpace
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

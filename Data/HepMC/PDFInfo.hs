module Data.HepMC.PDFInfo where

data PDFInfo = PDFInfo {
    pdfID1 :: Int,
    pdfID2 :: Int,
    pdfX1 :: Double,
    pdfX2 :: Double,
    qScale :: Double,
    pdfXfx1 :: Double,
    pdfXfx2 :: Double,
    pdfSetID1 :: Int,
    pdfSetID2 :: Int
} deriving (Eq, Ord, Read, Show)


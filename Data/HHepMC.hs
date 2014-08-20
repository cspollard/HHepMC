{-# LANGUAGE OverloadedStrings #-}

module Data.HHepMC where

import Debug.Trace (trace)
import Data.LorentzVector

import qualified Data.IntMap as IM
import Data.IntMap (IntMap)

import qualified Data.Attoparsec.Text.Lazy as APT
import Data.Attoparsec.Text.Lazy

import qualified Data.Text.Lazy as TL
import qualified Data.Text as TS
import Data.Text (Text)

import Data.Char (isSpace)

import Control.Applicative ((<|>))

-- just to make things easier...
parseWithSpace :: Parser a -> Parser a
parseWithSpace p = do
    x <- p
    skipSpace

    return x


parseList :: Int -> Parser a -> Parser [a]
parseList n p = do
    count n (parseWithSpace p)

dec :: Parser Int
dec = parseWithSpace (signed decimal)

doub :: Parser Double
doub = parseWithSpace (signed double)


type Version = Text

versionParser :: Parser Version
versionParser = do
    string "HepMC::Version"
    skipSpace
    x <- takeTill isEndOfLine
    skipSpace

    return $ trace "versionParser" $ x


data EventInfo = EventInfo {
    eventNumber :: Int,
    nMultPartInts :: Int,
    eventScale :: Double,
    alphaQCD :: Double,
    alphaQED :: Double,
    signalProcessID :: Int,
    signalProcessBarcode :: Int,
    nVertices :: Int,
    beamParticleBarcodes :: (Int, Int),
    nRndmStateInts :: Int,
    rndmStateInts :: [Int],
    nEventWeights :: Int,
    eventWeights :: [Double]
} deriving (Eq, Ord, Read, Show)

eventInfoParser :: Parser EventInfo
eventInfoParser = do
    char 'E'; skipSpace
    en <- dec
    nmpi <- dec
    scale <- doub
    aqcd <- doub
    aqed <- doub
    spid <- dec
    spbc <- dec
    nvtx <- dec
    bc1 <- dec
    bc2 <- dec
    let bpbcs = (bc1, bc2)

    nrs <- dec
    rs <- parseList nrs (signed decimal)

    nevtwgts <- dec
    evtwgts <- parseList nevtwgts $ (signed double)

    return (EventInfo en nmpi scale aqcd aqed spid spbc nvtx bpbcs nrs rs nevtwgts evtwgts)


type WeightNames = [Text]

weightNamesParser :: Parser WeightNames
weightNamesParser = do
    char 'N'
    skipSpace
    (signed decimal)
    skipSpace
    s <- takeTill isEndOfLine

    return $ trace "weightNamesParser" $
        TS.splitOn "\" \"" s


data UnitEnergy = MEV | GEV deriving (Eq, Ord, Read, Show)
data UnitLength = MM | CM deriving (Eq, Ord, Read, Show)

data Units = Units {
    unitEnergy :: UnitEnergy,
    unitLength :: UnitLength
} deriving (Eq, Ord, Read, Show)

unitParser :: Parser Units
unitParser = do
    return $ trace "unitParser" ()
    char 'U'; skipSpace
    ue <- takeTill isSpace; skipSpace
    ul <- takeTill isSpace; skipSpace

    return (Units (read $ TS.unpack ue) (read $ TS.unpack ul))


type CrossSection = (Double, Double)

crossSectionParser :: Parser CrossSection
crossSectionParser = do
    return $ trace "crossSectionParser" ()
    char 'C'; skipSpace
    cs <- doub
    err <- doub

    return (cs, err)


data HeavyIonInfo = HeavyIonInfo {
    nHardScatters :: Int,
    nProjParts :: Int,
    nTargParts :: Int,
    nNNInts :: Int,
    nSpectNeuts :: Int,
    nSpectProts :: Int,
    nNNwoundColls :: Int,
    nNwoundNColls :: Int,
    nNwoundNwoundColls :: Int,
    collImpactParam :: Double,
    eventPlaneAzimuth :: Double,
    nucleonEccent :: Double,
    inelastCrossSec :: Double
} deriving (Eq, Ord, Read, Show)

heavyIonInfoParser :: Parser HeavyIonInfo
heavyIonInfoParser = do
    return $ trace "heavyIonInfoParser" ()
    char 'H'; skipSpace
    nhs <- dec
    npp <- dec
    ntp <- dec
    nnn <- dec
    nsn <- dec
    nsp <- dec
    nnnwc <- dec
    nnwnc <- dec
    nnwnwc <- dec
    cip <- doub
    epa <- doub
    necc <- doub
    ics <- doub

    return (HeavyIonInfo nhs npp ntp nnn nsn nsp nnnwc nnwnc nnwnwc cip epa necc ics)


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

pdfInfoParser :: Parser PDFInfo
pdfInfoParser = do
    return $ trace "pdfInfoParser" ()
    char 'F'; skipSpace
    id1 <- dec
    id2 <- dec
    x1 <- doub
    x2 <- doub
    q <- doub
    xfx1 <- doub
    xfx2 <- doub
    set1 <- dec
    set2 <- dec

    return (PDFInfo id1 id2 x1 x2 q xfx1 xfx2 set1 set2)

data Vertex = Vertex {
    vertexBarcode :: Int,
    vertexID :: Int,
    vertexFourVec :: XYZT,
    nOrphan :: Int,
    nOutgoing :: Int,
    nVertexWeights :: Int,
    vertexWeights :: [Double]
} deriving (Eq, Ord, Read, Show)

vertexParser :: Parser Vertex
vertexParser = do
    return $ trace "vertexParser" ()
    char 'V'; skipSpace
    vtxbc <- dec
    vtxid <- dec
    x <- doub
    y <- doub
    z <- doub
    t <- doub

    let vec = XYZT x y z t

    norph <- dec
    nout <- dec
    nvtxwgt <- dec
    vtxwgts <- parseList nvtxwgt (signed double)

    return (Vertex vtxbc vtxid vec norph nout nvtxwgt vtxwgts)

    
data Particle = Particle {
    partBarcode :: Int,
    pdgID :: Int,
    partFourVec :: XYZT,
    partM :: Double,
    partStatus :: Int,
    polarizationTheta :: Double,
    polarizationPhi :: Double,
    parentVertexBarcode :: Int,
    nFlows :: Int,
    flows :: [(Int, Int)]
} deriving (Eq, Ord, Read, Show)

particleParser :: Parser Particle
particleParser = do
    return $ trace "particleParser" ()
    char 'P'; skipSpace
    bc <- dec
    pdgid <- dec

    x <- doub
    y <- doub
    z <- doub
    t <- doub

    let vec = XYZT x y z t

    m <- doub
    stat <- dec
    ptheta <- doub
    pphi <- doub
    pvbc <- dec
    nf <- dec
    
    let f = do
        s <- dec
        t <- dec
        return (s, t)

    fs <- parseList nf f

    return (Particle bc pdgid vec m stat ptheta pphi pvbc nf fs)


data Event = Event {
    eventInfo :: EventInfo,
    weightNames :: WeightNames,
    units :: Units,
    crossSection :: Maybe CrossSection,
    heavyIonInfo :: Maybe HeavyIonInfo,
    pdfInfo :: Maybe PDFInfo,
    vertices :: IntMap Vertex,
    particles :: IntMap Particle
} deriving (Eq, Ord, Read, Show)

eventParser :: Parser Event
eventParser = do
    -- parse info
    ei <- eventInfoParser; skipSpace

    -- parse units, weights, xsecs, heavy ions, pdfs
    wns <- weightNamesParser; skipSpace
    uns <- unitParser; skipSpace
    xsecs <- crossSectionParser; skipSpace
    -- hii <- heavyIonInfoParser; skipSpace
    pi <- pdfInfoParser; skipSpace

    -- parse vertices and particles
    many' $ (char 'V' <|> char 'P') >> takeTill isEndOfLine
    return $ trace "eventParser" $
        Event ei wns uns (Just xsecs) Nothing (Just pi) IM.empty IM.empty


-- todo
-- Info tagged union
-- choice



data HepMC = HepMC {
    version :: Version,
    events :: [Event]
} deriving (Eq, Ord, Read, Show)

hepMCParser :: Parser HepMC
hepMCParser = do
    skipSpace
    v <- versionParser

    string "HepMC::IO_GenEvent-START_EVENT_LISTING"; skipSpace
    evts <- many' eventParser

    return $ trace "hepMCParser" HepMC v evts

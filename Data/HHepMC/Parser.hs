{-# LANGUAGE OverloadedStrings #-}

module Data.HHepMC.Parser where

import Data.HHepMC.Parser.Utils

import Data.LorentzVector

import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import qualified Data.Map as M

import Data.Maybe (fromJust)

import Data.Attoparsec.Text.Lazy

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

import Data.Char (isSpace)

import Control.Applicative (Alternative(..))

-- just to make things easier...
type Version = TL.Text

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

type WeightNames = [TL.Text]

data UnitEnergy = MEV | GEV deriving (Eq, Ord, Read, Show)
data UnitLength = MM | CM deriving (Eq, Ord, Read, Show)

data Units = Units {
    unitEnergy :: UnitEnergy,
    unitLength :: UnitLength
} deriving (Eq, Ord, Read, Show)

type CrossSection = (Double, Double)

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

data Vertex = Vertex {
    vertexBarcode :: Int,
    vertexID :: Int,
    vertexFourVec :: XYZT,
    nOrphan :: Int,
    nOutgoing :: Int,
    nVertexWeights :: Int,
    vertexWeights :: [Double],
    particles :: [Particle]
} deriving (Eq, Ord, Read, Show)

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


parseVersion :: Parser Version
parseVersion = do
    string "HepMC::Version"; skipSpace
    toEndLine


parseBeginEventsLine :: Parser ()
parseBeginEventsLine = do
    string "HepMC::IO_GenEvent-START_EVENT_LISTING"
    skipSpace


parseEndEventsLine :: Parser ()
parseEndEventsLine = do
    string "HepMC::IO_GenEvent-END_EVENT_LISTING"
    skipSpace


parseEventInfo :: Parser EventInfo
parseEventInfo = do
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

    skipSpace

    return $
        EventInfo en nmpi scale aqcd aqed spid spbc nvtx bpbcs nrs rs nevtwgts evtwgts

parseWeightNames :: Parser WeightNames
parseWeightNames = do
    n <- decimal
    s <- count n parseQuote

    return s

parseUnits :: Parser Units
parseUnits = do
    ue <- takeTill isSpace
    skipSpace
    ul <- takeText

    return $
        Units (read $ TS.unpack ue) (read $ TS.unpack ul)

parseCrossSection :: Parser CrossSection
parseCrossSection = do
    cs <- doub
    err <- doub

    return (cs, err)


parseHeavyIonInfo :: Parser HeavyIonInfo
parseHeavyIonInfo = do
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

    return $
        HeavyIonInfo nhs npp ntp nnn nsn nsp nnnwc nnwnc nnwnwc cip epa necc ics


parsePDFInfo :: Parser PDFInfo
parsePDFInfo = do
    id1 <- dec
    id2 <- dec
    x1 <- doub
    x2 <- doub
    q <- doub
    xfx1 <- doub
    xfx2 <- doub
    set1 <- dec
    set2 <- dec

    return $
        PDFInfo id1 id2 x1 x2 q xfx1 xfx2 set1 set2


parseVertex :: Parser Vertex
parseVertex = do
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

    parts <- many parseParticle

    return $
        Vertex vtxbc vtxid vec norph nout nvtxwgt vtxwgts parts


parseParticle :: Parser Particle
parseParticle = do
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

    return $
        Particle bc pdgid vec m stat ptheta pphi pvbc nf fs


data EventHeader = EventHeader {
    eventInfo :: EventInfo,
    weightNames :: Maybe WeightNames,
    units :: Units,
    crossSection :: Maybe CrossSection,
    heavyIonInfo :: Maybe HeavyIonInfo,
    pdfInfo :: Maybe PDFInfo
} deriving (Eq, Ord, Read, Show)


parseHeaderLine :: Parser (Char, TL.Text)
parseHeaderLine = do
    k <- satisfy $ inClass "NUCHF"; skipSpace
    r <- toEndLine

    return (k, r)


parseEventHeader :: Parser EventHeader
parseEventHeader = do
    ei <- parseEventInfo
    ls <- many parseHeaderLine
    let m = M.fromList ls

    let wn = maybeResult . parse parseWeightNames =<< M.lookup 'N' m
    let u = fromJust $ maybeResult . parse parseUnits =<< M.lookup 'U' m
    let cs = maybeResult . parse parseCrossSection =<< M.lookup 'C' m
    let hii = maybeResult . parse parseHeavyIonInfo =<< M.lookup 'H' m
    let pdfi = maybeResult . parse parsePDFInfo =<< M.lookup 'F' m

    return $ EventHeader ei wn u cs hii pdfi


data Event = Event {
    eventHeader :: EventHeader,
    eventVertices :: [Vertex]
} deriving (Eq, Ord, Read, Show)


parseEvent :: Parser Event
parseEvent = do
    header <- parseEventHeader
    verts <- many parseVertex

    return $ Event header verts


data HepMC = HepMC {
    version :: Version,
    events :: [Event]
} deriving (Eq, Ord, Read, Show)


hepMCParser :: Parser HepMC
hepMCParser = do
    skipSpace
    v <- parseVersion
    parseBeginEventsLine

    evts <- many parseEvent

    parseEndEventsLine

    return $ HepMC v evts

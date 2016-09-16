{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Data.HepMC.Event
    ( module X
    , Event(..)
    , eobjs
    , parserEvent
    ) where

import Control.Lens

import qualified Data.Graph as G
import qualified Data.IntMap as IM

import Data.HepMC.Internal

import Data.HEP.LorentzVector as X
import Data.HepMC.Parse
import Data.HepMC.Vertex as X
import Data.HepMC.EventHeader as X


data GraphObj = V Vertex
              | P Particle
              deriving Show

makePrisms ''GraphObj

data Event =
    Event
        { _eobjs :: IM.IntMap GraphObj
        -- eventInfo :: EventInfo,
        -- weightNames :: Maybe [Text],
        -- units :: Units,
        -- crossSection :: Maybe CrossSection,
        -- heavyIonInfo :: Maybe HeavyIonInfo,
        -- pdfInfo :: Maybe PDFInfo
        } deriving Show


makeLenses ''Event

parserXYZT :: Parser XYZT
parserXYZT = XYZT
    <$> double <* skipSpace
    <*> double <* skipSpace
    <*> double <* skipSpace
    <*> double


-- parse the vertex barcode and the vertex.
parseRawVertex :: Parser ((Int, RawVertex), [Int] -> [(Int, Int)])
parseRawVertex = flip (<?>) "parseRawVertex" $ do
    char 'V' >> skipSpace
    vbc <- signed decimal <* skipSpace <?> "vertBC"
    v <- RawVertex vbc
            <$> (signed decimal <* skipSpace <?> "vertID")
            <*> (parserXYZT <* skipSpace <?> "vertXYZT")
            <*> (decimal <* skipSpace <?> "vertNOrphan")
            <*> (decimal <* skipSpace <?> "vertNOutgoing")
            <*> (hepmcList double <* endOfLine <?> "vertWeights")

    return ((vbc, v), fmap (vbc,))


parseRawParticle :: Parser ((Int, RawParticle), [(Int, Int)])
parseRawParticle = flip (<?>) "parseRawParticle" $ do
    char 'P' >> skipSpace
    pbc <- signed decimal <* skipSpace
    p <- RawParticle pbc
            <$> signed decimal <* skipSpace
            <*> parserXYZT <* skipSpace
            <*> double <* skipSpace
            <*> signed decimal <* skipSpace
            <*> double <* skipSpace
            <*> double <* skipSpace

    vbc <- signed decimal <* skipSpace
    p' <- p <$> hepmcList (tuple (signed decimal) (signed decimal)) <* endOfLine
    return ((pbc, p'), [(pbc, vbc)]) 



parserEvent :: Parser Event
parserEvent = do
    _ <- many parseHeaderLine
    (vs, pps, ees) <- fmap unzip3 <$> many $ do
            (v, vef) <- parseRawVertex
            (ps, pes) <- unzip <$> many parseRawParticle
            let ves = vef $ fmap fst ps
            return (v, ps, ves++concat pes)

    let ps = concat pps
    let is = fmap fst vs ++ fmap fst ps
    let mx = maximum is
    let mn = minimum is
    let g = G.buildG (mn, mx) $ concat ees
    let g' = G.transposeG g
    let gos = (fmap.fmap) (V . Vertex g g') vs
                ++ (fmap.fmap) (P . Particle g g') ps

    return $ Event (IM.fromList gos)

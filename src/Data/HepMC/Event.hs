{-# LANGUAGE TemplateHaskell #-}

module Data.HepMC.Event
    ( module X
    , Event(..)
    , graph
    , parserEvent
    ) where

import Control.Lens

import qualified Data.IntMap as IM

import qualified Data.Set as S

import Data.HEP.LorentzVector as X
import Data.HepMC.Parse
import Data.HepMC.Vertex as X
import Data.HepMC.EventHeader as X
import Data.HepMC.EventGraph as X

data Event =
    Event
    { _graph :: EventGraph
    -- eventInfo :: EventInfo,
    -- weightNames :: Maybe [Text],
    -- units :: Units,
    -- crossSection :: Maybe CrossSection,
    -- heavyIonInfo :: Maybe HeavyIonInfo,
    -- pdfInfo :: Maybe PDFInfo
    } deriving Show


makeLenses ''Event

instance HasVertices Event where
    vertices = graph . vertices

instance HasParticles Event where
    particles = graph . particles

parserXYZT :: Parser XYZT
parserXYZT = XYZT
    <$> double <* skipSpace
    <*> double <* skipSpace
    <*> double <* skipSpace
    <*> double


-- parse the vertex barcode and the vertex.
toVertex :: Parser (Int, Particles -> Particles -> Vertex)
toVertex = flip (<?>) "toVertex" $ do
    char 'V' >> skipSpace
    vbc <- signed decimal <* skipSpace <?> "vertBC"
    v <- Vertex vbc
        <$> (signed decimal <* skipSpace <?> "vertID")
        <*> (parserXYZT <* skipSpace <?> "vertXYZT")
        <*> (decimal <* skipSpace <?> "vertNOrphan")
        <*> (decimal <* skipSpace <?> "vertNOutgoing")
        <*> (hepmcList double <* endOfLine <?> "vertWeights")

    return (vbc, v)


toParticle :: Parser (Int, Int, Vertex -> Maybe Vertex -> Particle)
toParticle = flip (<?>) "toParticle" $ do
    char 'P' >> skipSpace
    pbc <- signed decimal <* skipSpace
    p <- Particle pbc
        <$> signed decimal <* skipSpace
        <*> parserXYZT <* skipSpace
        <*> double <* skipSpace
        <*> signed decimal <* skipSpace
        <*> double <* skipSpace
        <*> double <* skipSpace

    vbc <- signed decimal <* skipSpace
    p' <- p <$> hepmcList (tuple (signed decimal) (signed decimal)) <* endOfLine
    return (pbc, vbc, p')


eventGraph :: Parser EventGraph
eventGraph = do
    (vertFs, partFs, vertPs, vertDs, partP, partD) <-
            f (IM.empty, IM.empty, IM.empty, IM.empty, IM.empty, IM.empty)

    let (vs, ps) = let
            maybeInt x = if x == 0 then Nothing else Just x
            vertIM = IM.mapWithKey (\k v -> v (S.fromList . map (partIM IM.!) $ (vertPs IM.! k)) (S.fromList . map (partIM IM.!) $ (vertDs IM.! k))) vertFs
            partIM = IM.mapWithKey (\k p -> p (vertIM IM.! (partP IM.! k)) ((vertIM IM.!) <$> maybeInt (partD IM.! k))) partFs
            in (vertIM, partIM)

    return $ EventGraph (S.fromList $ IM.elems vs) (S.fromList $ IM.elems ps) vs ps

    where
        -- TODO
        -- certainly could be improved.
        -- loop over particles instead of unzip3
        f (vfs, pfs, vps, vds, pps, pds) = do
            (vbc, tv) <- toVertex
            (pbcs, vbcs, tps) <- unzip3 <$> many toParticle

            let vfs' = IM.insert vbc tv vfs
            let pfs' = IM.union pfs $ IM.fromList (zip pbcs tps)
            -- TODO
            -- problem here?
            let vps' = IM.unionWith (++) vps $ IM.fromList (zip vbcs $ map (:[]) pbcs)
            let vds' = IM.insert vbc pbcs vds
            let pps' = IM.union pps $ IM.fromList (zip pbcs $ repeat vbc)
            let pds' = IM.union pds $ IM.fromList (zip pbcs vbcs)

            let x = (vfs', pfs', vps', vds', pps', pds')

            f x <|> return x


parserEvent :: Parser Event
parserEvent = do
    _ <- many parseHeaderLine
    Event <$> eventGraph

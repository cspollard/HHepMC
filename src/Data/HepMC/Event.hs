module Data.HepMC.Event where

import Data.Either (partitionEithers)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Control.Monad.Fix (MonadFix(..))

import Data.HEP.LorentzVector

import Data.HepMC.Parse
import Data.HepMC.Barcoded
import Data.HepMC.Vertex
import Data.HepMC.EventHeader
import Data.HepMC.EventGraph

import Debug.Trace

data Event = Event {
    -- eventInfo :: EventInfo,
    -- weightNames :: Maybe [Text],
    -- units :: Units,
    -- crossSection :: Maybe CrossSection,
    -- heavyIonInfo :: Maybe HeavyIonInfo,
    -- pdfInfo :: Maybe PDFInfo
    graph :: EventGraph
}


parserXYZT :: Parser XYZT
parserXYZT = XYZT
    <$> signed double <* skipSpace
    <*> signed double <* skipSpace
    <*> signed double <* skipSpace
    <*> signed double


-- parse the vertex barcode and the vertex.
toVertex :: Parser (Int, Particles -> Particles -> Vertex)
toVertex = flip (<?>) "toVertex" $ do
    char 'V' >> skipSpace
    vbc <- signed decimal <* skipSpace <?> "vertBC"
    v <- Vertex vbc
        <$> (signed decimal <* skipSpace <?> "vertID")
        <*> (parserXYZT <* skipSpace <?> "vertXYZT")
        <*> (signed decimal <* skipSpace <?> "vert dec")
        <*> (signed decimal <* skipSpace <?> "vert dec")
        <*> (hepmcList (signed double) <* endOfLine <?> "vert hepmcList")

    return (vbc, v)


toParticle :: Parser (Int, Int, Vertex -> Vertex -> Particle)
toParticle = flip (<?>) "toParticle" $ do
    char 'P' >> skipSpace
    pbc <- signed decimal <* skipSpace
    p <- Particle pbc
        <$> signed decimal <* skipSpace
        <*> parserXYZT <* skipSpace
        <*> signed double <* skipSpace
        <*> signed decimal <* skipSpace
        <*> signed double <* skipSpace
        <*> signed double <* skipSpace

    vbc <- signed decimal <* skipSpace
    p' <- p <$> hepmcList (tuple (signed decimal) (signed decimal)) <* endOfLine
    return (pbc, vbc, p')


eventGraph :: Parser EventGraph
eventGraph = do
    (vertFs, partFs, vertPs, vertDs, partP, partD) <-
            f (IM.empty, IM.empty, IM.empty, IM.empty, IM.empty, IM.empty)

    let (vs, ps) = let
            vertIM = IM.mapWithKey (\k v -> v (map (partIM IM.!) (vertPs IM.! k)) (map (partIM IM.!) (vertDs IM.! k))) vertFs
            partIM = IM.mapWithKey (\k p -> p (vertIM IM.! (partP IM.! k)) (vertIM IM.! (partD IM.! k))) partFs
            in (vertIM, partIM)

    return $ EventGraph (IM.elems vs) (IM.elems ps) vs ps

    where
        -- certainly could be improved.
        -- loop over particles...
        f (vfs, pfs, vps, vds, pps, pds) = do
            (vbc, tv) <- toVertex
            (pbcs, vbcs, tps) <- unzip3 <$> many toParticle

            let vfs' = IM.insert vbc tv vfs
            let pfs' = IM.union pfs $ IM.fromList (zip pbcs tps)
            -- problem here.
            let vps' = IM.unionWith (++) vps $ IM.fromList (zip vbcs $ map (:[]) pbcs)
            let vds' = IM.insert vbc pbcs vds
            let pps' = IM.union pps $ IM.fromList (zip pbcs $ repeat vbc)
            let pds' = IM.union pds $ IM.fromList (zip pbcs vbcs)

            let x = (vfs', pfs', vps', vds', pps', pds')

            f x <|> return x


parserEvent :: Parser Event
parserEvent = do
    r <- many parseHeaderLine
    Event <$> eventGraph

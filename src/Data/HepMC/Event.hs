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
    <$> double <* skipSpace
    <*> double <* skipSpace
    <*> double <* skipSpace
    <*> double


-- parse the vertex barcode and the vertex.
toVertex :: Parser (Int, Particles -> Particles -> Vertex)
toVertex = do
    char 'V' >> skipSpace
    vbc <- decimal <* skipSpace
    v <- Vertex
        <$> return vbc
        <*> decimal <* skipSpace
        <*> parserXYZT <* skipSpace
        <*> decimal <* skipSpace
        <*> decimal <* skipSpace
        <*> decimal <* skipSpace
        <*> hepmcList double

    return (vbc, v)


toParticle :: Parser (Int, Int, Vertex -> Vertex -> Particle)
toParticle = do
    char 'P' >> skipSpace
    pbc <- decimal <* skipSpace
    p <- Particle
        <$> return pbc
        <*> decimal <* skipSpace
        <*> parserXYZT <* skipSpace
        <*> double <* skipSpace
        <*> decimal <* skipSpace
        <*> double <* skipSpace
        <*> double <* skipSpace

    vbc <- decimal <* skipSpace
    p' <- p <$> hepmcList (tuple decimal decimal)
    return (pbc, vbc, p')


-- toVertexParticle :: (IntMap (Particles -> Particles -> Vertex), IntMap (Vertex -> Vertex -> Particle), IntMap Int, IntMap


eventGraph :: (IntMap Vertex, IntMap Particle, IntMap Particles) ->
                Parser (IntMap Vertex, IntMap Particle, IntMap Particles)
eventGraph (imv, imp, imps) = do
    (vbc, tv) <- toVertex
    tps <- many toParticle
    return $
        let v = tv (map snd ps) (imps IM.! vbc)
            (ps, ps') = unzip $ flip map tps (\(pbc, vbc', tp) ->
                            let p = tp v (imv IM.! vbc') in ((pbc, p), (vbc', [p])))

        in (IM.insert vbc v imv,
            IM.union imp (IM.fromList ps),
            IM.unionWith (++) imps (IM.fromList ps'))


parserEvent :: Parser Event
parserEvent = do
    _ <- many parseHeaderLine
    (imv, imp, _) <- let x = eventGraph =<< x in x
    return $ Event $ EventGraph (IM.elems imv) (IM.elems imp) imv imp

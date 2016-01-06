module Data.HepMC.Event where

import Data.HepMC.Parse
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


toVertex :: IntMap Particle -> Parser Vertex
toParticle :: IntMap Vertex -> Parser Particle

eventGraph :: Parser EventGraph
eventGraph = do
        (vs, ps) <- partitionEithers <$> either (toVertex imP) (toParticle imV)
        EventGraph <$> vs <*> ps <*> imV <*> imP

    where
        imV = IM.fromList . map (\v -> (bc v, v)) $ vs
        imP = IM.fromList . map (\p -> (bc p, p)) $ ps


parserEvent :: Parser Event
parserEvent = do
    _ <- many parseHeaderLine
    Event <$> eventGraph

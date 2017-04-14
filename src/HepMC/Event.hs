{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module HepMC.Event
    ( module X
    , Event(..)
    , eparts, everts, graph, graph'
    , parserEvent
    , Vertex
    , vertID, vertNOrphan
    , vertNOutgoing, vertWeights
    , vevent
    , Particle
    , partM, partStatus
    , partPolarizationTheta, partPolarizationPhi
    , partFlows, pevent
    ) where

import           Data.Attoparsec.ByteString.Char8 as X hiding (parse)
import           Control.Lens
import qualified Data.Graph             as G
import           Data.HEP.LorentzVector as X
import           Data.HEP.PID
import qualified Data.IntMap            as IM
import           HepMC.Barcoded
import           HepMC.Internal
import           HepMC.Parse

data Vertex =
  Vertex
    { _vevent :: Event
    , _rvert  :: RawVertex
    }

data Particle =
  Particle
    { _pevent :: Event
    , _rpart  :: RawParticle
    }

instance Show Vertex where
  show = show . _rvert

instance Show Particle where
  show = show . _rpart

data Event =
  Event
    { _eparts :: IM.IntMap Particle
    , _everts :: IM.IntMap Vertex
    , _graph  :: G.Graph
    , _graph' :: G.Graph
    -- eventInfo :: EventInfo,
    -- weightNames :: Maybe [Text],
    -- units :: Units,
    -- crossSection :: Maybe CrossSection,
    -- heavyIonInfo :: Maybe HeavyIonInfo,
    -- pdfInfo :: Maybe PDFInfo
    } deriving Show


makeLenses ''Vertex
makeLenses ''Particle
makeLenses ''Event

vertID :: Lens' Vertex Int
vertID = rvert . rvertID

vertNOrphan :: Lens' Vertex Int
vertNOrphan = rvert . rvertNOrphan

vertNOutgoing :: Lens' Vertex Int
vertNOutgoing = rvert . rvertNOutgoing

vertWeights :: Lens' Vertex [Double]
vertWeights = rvert . rvertWeights


partM :: Lens' Particle Double
partM = rpart . rpartM

partStatus :: Lens' Particle Int
partStatus = rpart . rpartStatus

partPolarizationTheta :: Lens' Particle Double
partPolarizationTheta = rpart . rpartPolarizationTheta

partPolarizationPhi :: Lens' Particle Double
partPolarizationPhi = rpart . rpartPolarizationPhi

partFlows :: Lens' Particle [(Int, Int)]
partFlows = rpart . rpartFlows


instance Barcoded Vertex where
  bc = rvert . rvertBC

instance Eq Vertex where
  (==) = liftBC2 (==)

instance Ord Vertex where
  compare = liftBC2 compare

instance HasLorentzVector Vertex where
  toXYZT = rvert . rvertXYZT

instance Barcoded Particle where
  bc = rpart . rpartBC

instance Eq Particle where
  (==) = liftBC2 (==)

instance Ord Particle where
  compare = liftBC2 compare

instance HasLorentzVector Particle where
  toXYZT = rpart . rpartXYZT

instance HasPID Particle where
  pid = rpart . rpartPID


parserXYZT :: Parser XYZT
parserXYZT =
  XYZT
    <$> double <* skipSpace
    <*> double <* skipSpace
    <*> double <* skipSpace
    <*> double


-- parse the vertex barcode and the vertex.
parseRawVertex :: Parser ((Int, RawVertex), [Int] -> [(Int, Int)])
parseRawVertex =
  flip (<?>) "parseRawVertex" $ do
    char 'V' >> skipSpace
    vbc <- signed decimal <* skipSpace <?> "vertBC"
    v <-
      RawVertex vbc
        <$> (signed decimal <* skipSpace <?> "vertID")
        <*> (parserXYZT <* skipSpace <?> "vertXYZT")
        <*> (decimal <* skipSpace <?> "vertNOrphan")
        <*> (decimal <* skipSpace <?> "vertNOutgoing")
        <*> (vector double <* endOfLine <?> "vertWeights")

    return ((vbc, v), fmap (vbc,))


parseRawParticle :: Parser ((Int, RawParticle), [(Int, Int)])
parseRawParticle =
  flip (<?>) "parseRawParticle" $ do
    char 'P' >> skipSpace
    pbc <- signed decimal <* skipSpace
    p <-
      RawParticle pbc
        <$> signed decimal <* skipSpace
        <*> parserXYZT <* skipSpace
        <*> double <* skipSpace
        <*> signed decimal <* skipSpace
        <*> double <* skipSpace
        <*> double <* skipSpace

    vbc <- signed decimal <* skipSpace
    p' <- p <$> vector (tuple (signed decimal) (signed decimal)) <* endOfLine
    return ((pbc, p'), if vbc == 0 then [] else [(pbc, vbc)])



parserEvent :: Parser Event
parserEvent = do
  -- TODO
  -- event info...
  _ <- many parseHeaderLine
  -- TODO
  -- event should have many1 vertices?
  (vs, pps, ees) <-
    fmap unzip3 <$> many1 $ do
      (v, vef) <- parseRawVertex
      (ps, pes) <- unzip <$> many parseRawParticle
      let ves = vef $ fmap fst ps
      return (v, ps, ves++concat pes)

  let ps = concat pps
      rparts = IM.fromList ps
      rverts = IM.fromList vs

      is = fmap fst vs ++ fmap fst ps
      mx = maximum is
      mn = minimum is
      g = G.buildG (mn, mx) $ concat ees
      g' = G.transposeG g

      partmap = Particle evt <$> rparts
      vertmap = Vertex evt <$> rverts
      evt = Event partmap vertmap g g'

  return evt

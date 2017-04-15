{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module HepMC.Event
    ( module X
    , Event
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

import           Control.Lens
import           Data.Attoparsec.ByteString.Char8 as X hiding (parse)
import qualified Data.Graph                       as G
import           Data.HEP.LorentzVector           as X
import           Data.HEP.PID
import qualified Data.IntMap                      as IM
import qualified Data.Map.Strict                  as M
import           Data.Text                        (Text)
import           HepMC.Barcoded
import           HepMC.EventHeader                as X
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


data Event =
  Event
    { _evtparts  :: IM.IntMap Particle
    , _evtverts  :: IM.IntMap Vertex
    , _evtgraph  :: G.Graph
    , _evtgraph' :: G.Graph
    , _evtheader :: EventHeader
    }


makeLenses ''Vertex
makeLenses ''Particle
makeLenses ''Event

instance Show Vertex where
  show = show . _rvert

instance Show Particle where
  show = show . _rpart

instance HasLorentzVector Vertex where
  toXYZT = rvert . rvertXYZT

instance HasLorentzVector Particle where
  toXYZT = rpart . rpartXYZT

instance HasPID Particle where
  pid = rpart . rpartPID


vertNOrphan :: Lens' Vertex Int
vertNOrphan = rvert . rvertNOrphan

vertNOutgoing :: Lens' Vertex Int
vertNOutgoing = rvert . rvertNOutgoing

vertWeights :: Lens' Vertex (Vector Double)
vertWeights = rvert . rvertWeights


partM :: Lens' Particle Double
partM = rpart . rpartM

partStatus :: Lens' Particle Int
partStatus = rpart . rpartStatus

partPolarizationTheta :: Lens' Particle Double
partPolarizationTheta = rpart . rpartPolarizationTheta

partPolarizationPhi :: Lens' Particle Double
partPolarizationPhi = rpart . rpartPolarizationPhi

partFlows :: Lens' Particle (Vector (Int, Int))
partFlows = rpart . rpartFlows




-- parse the vertex barcode and the vertex.
parseRawVertex :: Parser ((Int, RawVertex), [Int] -> [(Int, Int)])
parseRawVertex =
  flip (<?>) "parseRawVertex" $ do
    char 'V' >> skipSpace
    vbc <- signed decimal <* skipSpace <?> "rvertBC"
    v <-
      RawVertex vbc
        <$> (signed decimal <* skipSpace <?> "rvertID")
        <*> (xyzt <* skipSpace <?> "rvertXYZT")
        <*> (decimal <* skipSpace <?> "rvertNOrphan")
        <*> (decimal <* skipSpace <?> "rvertNOutgoing")
        <*> (vector double <* endOfLine <?> "rvertWeights")

    return ((vbc, v), fmap (vbc,))


parseRawParticle :: Parser ((Int, RawParticle), [(Int, Int)])
parseRawParticle =
  flip (<?>) "parseRawParticle" $ do
    char 'P' >> skipSpace
    pbc <- signed decimal <* skipSpace <?> "rpartBC"
    p <-
      RawParticle pbc
        <$> (signed decimal <* skipSpace <?> "rpartPID")
        <*> (xyzt <* skipSpace <?> "rpartXYZT")
        <*> (double <* skipSpace <?> "rpartM")
        <*> (signed decimal <* skipSpace <?> "rpartStatus")
        <*> (double <* skipSpace <?> "rpartPolarizationTheta")
        <*> (double <* skipSpace <?> "rpartPolarizationPhi")

    vbc <- signed decimal <* skipSpace <?> "rpartVertexBC"
    p' <-
      p <$> vector (tuple (signed decimal) (signed decimal)) <* endOfLine
        <?> "rpartFlows"
    return ((pbc, p'), if vbc == 0 then [] else [(pbc, vbc)])


parserEvent :: Parser Event
parserEvent = do
  eh <- parserEventHeader
  (vs, pps, ees) <-
    fmap unzip3 <$> many $ do
      (v, vef) <- parseRawVertex
      (ps, pes) <- unzip <$> many parseRawParticle
      let ves = vef $ fst <$> ps
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

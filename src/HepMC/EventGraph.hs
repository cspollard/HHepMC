{-# LANGUAGE TemplateHaskell #-}

module HepMC.EventGraph where

import           Control.Lens   hiding (children)
import qualified Data.Graph     as G
import qualified Data.IntMap    as IM
import           HepMC.Internal
import           HepMC.Parse


data EventGraph =
  EventGraph
    { _evtgraph  :: G.Graph
    , _evtgraph' :: G.Graph
    , _rawverts  :: IM.IntMap RawVertex
    , _rawparts  :: IM.IntMap RawParticle
    }

makeLenses ''EventGraph

parserEventGraph :: Parser EventGraph
parserEventGraph = do
  (vs, pps, ees) <-
    fmap unzip3 <$> many1 $ do
      (v, vef) <- parseRawVertex
      (ps, pes) <- unzip <$> many1 parseRawParticle
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

  return $ EventGraph g g' rverts rparts

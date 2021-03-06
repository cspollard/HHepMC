{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module HepMC.Event
    ( module X
    , Event(..)
    , parserEvent
    ) where

import           Control.Lens
import qualified Data.IntMap       as IM
import           HepMC.EventGraph
import           HepMC.EventHeader
import           HepMC.Parse
import           HepMC.Particle    as X


data Event =
  Event
    { particles :: [Particle]
    , vertices  :: [Vertex]
    , header    :: EventHeader
    }

parserEvent :: Parser Event
parserEvent = do
  eh <- parserEventHeader
  eg <- parserEventGraph
  let ps = Particle eg <$> IM.elems (view rawparts eg)
      vs = Vertex eg <$> IM.elems (view rawverts eg)

  return $ Event ps vs eh

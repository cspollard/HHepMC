module Main where

import           Control.Lens       hiding (children)
import           Data.HEP.PID
import           HepMC.Event
import           HepMC.Pipes
import           Pipes              ((>->))
import qualified Pipes              as P
import qualified Pipes.ByteString   as PB
import qualified Pipes.Prelude      as P
import           System.Environment (getArgs)
import           System.IO


main :: IO ()
main = do
  fname <- head <$> getArgs
  withFile fname ReadMode $ \f ->
    P.runEffect
    $ fromStream (PB.fromHandle f) >-> P.map particles >-> P.print


-- finalWith :: (Particle -> Bool) -> Particle -> Bool
-- finalWith f p = f p && (not . any f $ toListOf children p)
--
--
-- ewDecay :: Particle -> Bool
-- ewDecay =
--   or
--     . traverse finalWith
--       (classOf <$> [bottomHadron, charmHadron, tau, anti tau])

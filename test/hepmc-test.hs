module Main where

import           Control.Lens            hiding (children)

import           Conduit
import           Data.Conduit.Attoparsec

import           Data.HEP.PID

import           Data.HepMC.Event
import           Data.HepMC.EventGraph
import           Data.HepMC.Parse
import           System.Environment      (getArgs)


main :: IO ()
main = do
    f <- head <$> getArgs
    runResourceT $
            sourceFile f
            =$= (sinkParser parserVersion
                    >> conduitParserEither parserEvent)

            =$= catEithersC
            =$= mapC snd
            =$= mapC (toListOf (particles.filtered ewDecay.children.pid))
            -- =$= mapC (sortOn (negate . view lvPt))
            $$  mapM_C (liftIO . print)
            -- $$  mapM_C (liftIO . mapM_ (views bc print))
            -- $$  mapM_C (liftIO . findZll)

    where catEithersC = do
            x <- await
            case x of
                Just (Right y) -> yield y >> catEithersC
                _              -> return ()

finalWith :: (Particle -> Bool) -> Particle -> Bool
finalWith f p = f p && (not . any f $ toListOf children p)


ewDecay :: Particle -> Bool
ewDecay =
  or
    . traverse finalWith
      (classOf <$> [bottomHadron, charmHadron, tau, anti tau])

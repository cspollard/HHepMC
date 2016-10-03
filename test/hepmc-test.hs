module Main where

import Control.Lens hiding (children)

import Conduit
import Data.Conduit.Attoparsec

import Data.List (sortOn)

import Data.HEP.LorentzVector
import Data.HEP.PID

import Data.HepMC.Parse
import Data.HepMC.Barcoded
import Data.HepMC.Event
import Data.HepMC.EventGraph
import System.Environment (getArgs)


main :: IO ()
main = do
    f <- head <$> getArgs
    runResourceT $
            sourceFile f
            =$= (sinkParser parserVersion
                    >> conduitParserEither parserEvent)

            =$= catEithersC
            =$= mapC snd
            =$= mapC (toListOf (particles.filtered promptLepton))
            =$= mapC (sortOn (negate . view lvPt))
            $$  mapM_C (liftIO . mapM_ (views bc print))
            -- $$  mapM_C (liftIO . findZll)

    where catEithersC = do
            x <- await
            case x of
                Just (Right y) -> yield y >> catEithersC
                _              -> return ()


promptLepton :: Particle -> Bool
promptLepton = and . sequenceA [isChargedLepton, final, not . fromHadron]

findZll :: Event -> IO ()
findZll e = case e ^.. particles . filtered promptLepton of
                ls@[_, _] -> do
                        traverseOf_ (traverse . pid) print ls
                        views lvM print (foldOf (traverse . toXYZT) ls)

                _ -> print "nope"


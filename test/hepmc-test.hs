module Main where

import Control.Lens

import Conduit
import Data.Conduit.Attoparsec

import Data.HEP.LorentzVector
import Data.HEP.PID

import Data.HepMC.Parse
import Data.HepMC.Event
import Data.HepMC.EventGraph
import System.Environment (getArgs)




main :: IO ()
main = do
    f <- head <$> getArgs
    runResourceT $
            sourceFile f
            =$= (sinkParser parserVersion
                    >> conduitParser parserEvent)

            =$= mapC snd
            $$  mapM_C (liftIO . findZll)
            -- $$  mapM_C (liftIO . print . toListOf (particles . filtered final))


findZll :: Event -> IO ()
findZll e = case e ^.. particles . filtered promptLepton of
                ls@[_, _] -> do
                        traverseOf_ (traverse . pid) print ls
                        views lvM print (foldOf (traverse . toXYZT) ls)

                xs -> print xs

    where promptLepton = and . sequenceA [isChargedLepton, prompt, final, (> 25) . view lvPt]

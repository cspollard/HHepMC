module Main where

import Control.Lens hiding (children)

import Data.Either (isRight)

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
                    >> conduitParserEither parserEvent)

            =$= catEithersC
            =$= mapC snd
            -- $$  mapM_C (liftIO . print . toListOf (particles.filtered final))
            $$  mapM_C (liftIO . findZll)

    where catEithersC = do
            x <- await
            case x of
                Just (Right y) -> yield y >> catEithersC
                _              -> return ()


findZll :: Event -> IO ()
findZll e = case e ^.. particles . filtered promptLepton of
                ls@[_, _] -> do
                        -- traverseOf_ (traverse . pid) print ls
                        views lvM print (foldOf (traverse . toXYZT) ls)

                xs -> print "nope"

    where promptLepton = and . sequenceA [isChargedLepton, prompt, final]

module Main where

import Control.Lens
import qualified Data.Set as S

import Conduit
import Data.Conduit.Attoparsec

import Data.HEP.LorentzVector
import Data.HEP.PID

import Data.HepMC.Parse
import Data.HepMC.Event
import System.Environment (getArgs)




main :: IO ()
main = do
    f <- head <$> getArgs
    runResourceT $
            sourceFile f
            =$= (sinkParser parserVersion
                    >> conduitParser parserEvent)

            $$ mapM_C (liftIO . print)

            {-
            =$= mapC snd
            $$  mapM_C (liftIO . findZll)


findZll :: Event -> IO ()
findZll e = case e ^.. particles . to S.toList . traverse . filtered promptLepton of
                ls@[_, _] -> do
                        traverseOf_ (traverse . pid) print ls
                        views lvM print (foldOf (traverse . toPtEtaPhiE) ls)

                _           -> return ()

    where promptLepton = (&&) <$> isChargedLepton <*> prompt
    -}

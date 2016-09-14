module Main where

import Control.Lens
import Data.Monoid ((<>))
import qualified Data.Set as S

import Conduit
import qualified Data.Conduit.List as CL
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
            =$= mapC snd =$= mapC findZll
            =$= CL.catMaybes
            $$  mapM_C (liftIO . (\x -> print (x, view lvM x)))


findZll :: Event -> Maybe PtEtaPhiE
findZll e = case e ^.. particles . to S.toList . traverse . filtered promptLepton . toPtEtaPhiE of
                [l1, l2] -> Just $ l1 <> l2
                _        -> Nothing

    where promptLepton = (&&) <$> isChargedLepton <*> prompt

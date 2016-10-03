module Main where

import Debug.Trace

import Control.Lens hiding (children)

import Conduit
import Data.Conduit.Attoparsec

import Data.List (sortOn)
import Data.Ord (Down(..))

import Data.HEP.LorentzVector
import Data.HEP.PID

import Data.HepMC.Barcoded
import Data.HepMC.Parse
import Data.HepMC.Event
import Data.HepMC.EventGraph

import Data.Jet

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
            =$= mapC (sortOn (Down . view lvPt) . toListOf (particles . filtered finalHadron))
            $$  mapM_C (liftIO . (\xs -> print (length xs) >> views (traverse . bc) print xs))
            -- =$= mapC (sortOn (Down . view lvPt) . map (view toPtEtaPhiE . snd . obj) . cluster akt04)
            -- $$  mapM_C (liftIO . (\xs -> putStrLn "new event" >> mapM_ print xs))

    where catEithersC = do
            x <- await
            case x of
                Just (Right y) -> yield y >> catEithersC
                _              -> return ()


finalHadron :: Particle -> Bool
finalHadron = and . sequenceA [isHadron, final]

promptLepton :: Particle -> Bool
promptLepton = and . sequenceA [isChargedLepton, final, not . fromHadron]

akt04 :: Clustering Double
akt04 = Clustering d d0
    where
        d p p' = (view lvPt p `min` view lvPt p') ^^ (-1) * lvDR p p' / 0.4
        d0 p = view lvPt p ^^ (-1)

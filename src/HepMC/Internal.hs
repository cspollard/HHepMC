{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module HepMC.Internal where

import           Control.Lens
import           Data.HEP.LorentzVector
import           Data.HEP.PID
import           Data.Vector
import           HepMC.Barcoded
import           HepMC.Parse

data RawVertex =
  RawVertex
    { _rvertBC        :: Int
    , _rvertID        :: Int
    , _rvertXYZT      :: XYZT
    , _rvertNOrphan   :: Int
    , _rvertNOutgoing :: Int
    , _rvertWeights   :: Vector Double
    } deriving Show


data RawParticle =
  RawParticle
    { _rpartBC                :: Int
    , _rpartPID               :: PID
    , _rpartXYZT              :: XYZT
    , _rpartM                 :: Double
    , _rpartStatus            :: Int
    , _rpartPolarizationTheta :: Double
    , _rpartPolarizationPhi   :: Double
    , _rpartFlows             :: Vector (Int, Int)
    } deriving Show

makeLenses ''RawVertex
makeLenses ''RawParticle


instance Barcoded RawVertex where
  bc = rvertBC

instance Barcoded RawParticle where
  bc = rpartBC

instance Eq RawVertex where
  (==) = liftBC2 (==)

instance Eq RawParticle where
  (==) = liftBC2 (==)

instance Ord RawVertex where
  compare = liftBC2 compare

instance Ord RawParticle where
  compare = liftBC2 compare


-- parse the vertex barcode and the vertex.
parseRawVertex :: Parser ((Int, RawVertex), [Int] -> [(Int, Int)])
parseRawVertex =
  flip (<?>) "parseRawVertex" $ do
    char 'V' >> skipSpace
    vbc <- signed decimal <* skipSpace <?> "rvertBC"
    v <-
      RawVertex vbc
        <$> (signed decimal <* skipSpace <?> "rvertID")
        <*> (xyzt <* skipSpace <?> "rvertXYZT")
        <*> (decimal <* skipSpace <?> "rvertNOrphan")
        <*> (decimal <* skipSpace <?> "rvertNOutgoing")
        <*> (vector double <* endOfLine <?> "rvertWeights")

    return ((vbc, v), fmap (vbc,))


parseRawParticle :: Parser ((Int, RawParticle), [(Int, Int)])
parseRawParticle =
  flip (<?>) "parseRawParticle" $ do
    char 'P' >> skipSpace
    pbc <- signed decimal <* skipSpace <?> "rpartBC"
    p <-
      RawParticle pbc
        <$> (signed decimal <* skipSpace <?> "rpartPID")
        <*> (xyzt <* skipSpace <?> "rpartXYZT")
        <*> (double <* skipSpace <?> "rpartM")
        <*> (signed decimal <* skipSpace <?> "rpartStatus")
        <*> (double <* skipSpace <?> "rpartPolarizationTheta")
        <*> (double <* skipSpace <?> "rpartPolarizationPhi")

    vbc <- signed decimal <* skipSpace <?> "rpartVertexBC"
    p' <-
      p <$> vector (tuple (signed decimal) (signed decimal)) <* endOfLine
        <?> "rpartFlows"
    return ((pbc, p'), if vbc == 0 then [] else [(pbc, vbc)])

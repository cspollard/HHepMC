{-# LANGUAGE RankNTypes #-}

module HepMC.Pipes where

import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Data.ByteString        (ByteString)
import           Data.Maybe             (fromMaybe)
import           HepMC.Event
import           HepMC.Parse
import           Pipes
import qualified Pipes.Attoparsec       as PA
import qualified Pipes.Parse            as PP

-- TODO
-- this doesn't exit cleanly....
fromStream :: MonadIO m => Producer ByteString m x -> Producer Event m ()
fromStream p = do
  (evers, p') <- lift $ PP.runStateT (parseOne hmcvers) p
  case evers of
    Left s  -> liftIO $ print s
    Right v -> do
      liftIO . putStrLn $ "hepmc version: " ++ show v
      ex <- PA.parsed parserEvent p'
      case ex of
        Right _         -> liftIO $ print "no hepmc footer?!"
        Left (_, p'') -> do
          (exxx, _) <- lift $ PP.runStateT (parseOne hmcend) p''
          case exxx of
            Right _ -> liftIO $ putStrLn "finished."
            Left x  -> liftIO $ print x


parseOne
  :: Monad m
  => Parser b
  -> PP.Parser ByteString m (Either PA.ParsingError b)
parseOne p = do
  mex <- PA.parse p
  return $ fromMaybe (Left $ PA.ParsingError [] "no input!") mex

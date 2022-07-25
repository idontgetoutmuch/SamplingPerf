{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad.Primitive (RealWorld)
import Control.Monad.Reader (replicateM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT (..), ask, runReaderT)
import System.Random.MWC
  ( Gen,
    GenIO,
    createSystemRandom,
  )
import System.Random.Stateful (StatefulGen, uniformDouble01M)

class Monad m => MonadSample m where
  random :: m Double

newtype Sampler a = Sampler (ReaderT GenIO IO a)

runSampler :: Sampler a -> ReaderT GenIO IO a
runSampler (Sampler s) = s

sampleWith :: Sampler a -> Gen RealWorld -> IO a
sampleWith (Sampler m) = runReaderT m

instance Functor Sampler where
  fmap f (Sampler s) = Sampler $ fmap f s

instance Applicative Sampler where
  pure x = Sampler $ pure x
  (Sampler f) <*> (Sampler x) = Sampler $ f <*> x

instance Monad Sampler where
  (Sampler x) >>= f = Sampler $ x >>= runSampler . f

instance MonadSample Sampler where
  random = Sampler (ReaderT uniformDouble01M)

main :: IO ()
main = do
  g <- createSystemRandom
  xs <- sampleWith (replicateM 1000000 random) g
  print $ sum xs

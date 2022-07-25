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

-- The general version

newtype Sampler g m a = Sampler (StatefulGen g m => ReaderT g m a)

runSampler :: StatefulGen g m => Sampler g m a -> ReaderT g m a
runSampler (Sampler s) = s

sampleWith :: (StatefulGen g m) => Sampler g m a -> g -> m a
sampleWith (Sampler m) = runReaderT m

{-# SPECIALIZE sampleWith :: Sampler GenIO IO a -> GenIO -> IO a #-}

instance Functor (Sampler g m) where
  fmap f (Sampler s) = Sampler $ fmap f s

instance Applicative (Sampler g m) where
  pure x = Sampler $ pure x
  (Sampler f) <*> (Sampler x) = Sampler $ f <*> x

instance Monad (Sampler g m) where
  (Sampler x) >>= f = Sampler $ x >>= runSampler . f

instance MonadSample (Sampler g m) where
  random = Sampler (ReaderT uniformDouble01M)

-- A specialised version

newtype SamplerIO a = SamplerIO (ReaderT GenIO IO a)

runSamplerIO :: SamplerIO a -> ReaderT GenIO IO a
runSamplerIO (SamplerIO s) = s

sampleIOwith :: SamplerIO a -> Gen RealWorld -> IO a
sampleIOwith (SamplerIO m) = runReaderT m

instance Functor SamplerIO where
  fmap f (SamplerIO s) = SamplerIO $ fmap f s

instance Applicative SamplerIO where
  pure x = SamplerIO $ pure x
  (SamplerIO f) <*> (SamplerIO x) = SamplerIO $ f <*> x

instance Monad SamplerIO where
  (SamplerIO x) >>= f = SamplerIO $ x >>= runSamplerIO . f

instance MonadSample SamplerIO where
  random = SamplerIO $ ask >>= lift . uniformDouble01M

main :: IO ()
main = do
  g <- createSystemRandom
  xs <- sampleWith (replicateM 1000000 random) g
  print $ sum xs

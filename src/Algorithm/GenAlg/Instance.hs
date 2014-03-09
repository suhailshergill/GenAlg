{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types, FlexibleContexts, UndecidableInstances #-}

module Algorithm.GenAlg.Instance where

import Algorithm.GenAlg
import Data.Reflection -- from reflection
import Data.Proxy -- from tagged

-- | A dictionary describing a GenAlg instance
data GenAlgInstance a = GenAlgInstance {
  config_ :: GenAlgConfig a
  , crossover_ :: (Chromosome a)
               -> (Chromosome a)
               -> (Rand StdGen) ((Chromosome a), (Chromosome a))
  , mutation_ :: (Chromosome a)
              -> (Rand StdGen) (Chromosome a)
  , randomChromosome_ :: GenAlgConfig a
                      -> Rand StdGen (Chromosome a)
  , unfitnessScore_ :: (Chromosome a)
                       -> Rational
  }

newtype Population a s = Population {
  withPopulation :: EitherT [(Chromosome a)] (Rand StdGen) [(Chromosome a)]
  }

withGAInstance :: GenAlgConfig a
                  -> ((Chromosome a)
                      -> (Chromosome a)
                      -> (Rand StdGen) ((Chromosome a), (Chromosome a)))
                  -> ((Chromosome a)
                      -> (Rand StdGen) (Chromosome a))
                  -> (GenAlgConfig a
                      -> Rand StdGen (Chromosome a))
                  -> ((Chromosome a)
                       -> Rational)
                  -> (forall s. Reifies s (GenAlgInstance a) => Population a s)
                  -> EitherT [(Chromosome a)] (Rand StdGen) [(Chromosome a)]
withGAInstance = undefined

evolveWithGAInstance :: Reifies s (GenAlgInstance a)
                        => MaxIterations
                        -> Population a s
evolveWithGAInstance = undefined

newtype GA a s = GA { runGA :: a }

instance Reifies s (GenAlgInstance a)
         => GenAlg(GA a s) where
  config = undefined
  mutation = undefined
  crossover = undefined
  randomChromosome = undefined
  unfitnessScore = undefined

instance Reifies s (GenAlgInstance a)
         => GenAlgEvolution(GA a s) where
  foo :: IterationsLeft
         -> EitherT [(Chromosome (GA a s))] (Rand StdGen) [(Chromosome (GA a s))]
         -> EitherT [(Chromosome (GA a s))] (Rand StdGen) [(Chromosome (GA a s))]
  foo n pop = EitherT $ do
    startPop  <- runEitherT pop
    return $ Left []

-- withMonoid :: (a -> a -> a) -> a -> (forall s. Reifies s (Monoid_ a) => M a s) -> a
-- withMonoid f z v = reify (Monoid_ f z) (runM . asProxyOf v)

asProxyOf :: f s -> Proxy s -> f s
asProxyOf a _ = a

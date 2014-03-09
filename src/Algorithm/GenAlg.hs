{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Algorithm.GenAlg (
  GenAlg, Dom, Chromosome
  , IterationsLeft, MaxIterations
  , Length, Rate
  , mkGenAlgConfig, GenAlgConfig
  , minChromosomeLength, maxChromosomeLength, populationSize, crossRate,
    mutRate, maxIterations
  , MutationRate(..), CrossoverRate(..), MinLength(..), MaxLength(..), PopSize(..)
  , crossover, mutation, randomChromosome, unfitnessScore
  , evolveForTime, evolve
  , EitherT(..)
  , Rand
  , StdGen
  , Rational
  ) where

import Prelude hiding (head, undefined)

import Control.Lens
import Data.List
import Data.Maybe
import Data.Ratio
import Control.Monad
import Control.Monad.List
import Control.Monad.Trans.Either
import Control.Monad.Random


type Size = Int
type Length = Integer
type IterationsLeft = Integer
type MaxIterations = Integer
type Rate = Double

newtype PopSize = PopSize Size deriving (Show, Eq)
newtype MinLength = MinLength Length deriving (Show, Eq)
newtype MaxLength = MaxLength Length deriving (Show, Eq)
newtype MutationRate = MutationRate Rate deriving (Show, Eq)
newtype CrossoverRate = CrossoverRate Rate deriving (Show, Eq)


-- | Generic representation of a Chromosome
data family Chromosome a


-- | General settings for GenAlg
data GenAlgConfig repr range = GenAlgConfig {
  -- | minimum allowed chromosome length (inclusive)
  _minChromosomeLength :: MinLength
  -- | maximum allowed chromosome length (inclusive)
  , _maxChromosomeLength :: MaxLength
  -- | population size
  , _populationSize :: PopSize
  -- | crossover likelihood
  , _crossRate :: CrossoverRate
  -- | mutation likelihood
  , _mutRate :: MutationRate
  -- | The maximum number of iterations for which to run 'evolveForTime'
  , _maxIterations :: MaxIterations
  -- | The 'unfitnessScore' of a chromosome should be inversely correlated with
  -- that chromosome's fitness. Unfitness, by definition, is always finite and
  -- should always be non-negative.
  , _unfitnessScore :: Chromosome repr -> Rational
  }
makeLenses ''GenAlgConfig

-- | Safely construct a 'GenAlgConfig repr range'
mkGenAlgConfig :: MinLength
               -> MaxLength
               -> PopSize
               -> CrossoverRate
               -> MutationRate
               -> MaxIterations
               -> (Chromosome repr -> Rational)
               -> Maybe (GenAlgConfig repr range)
mkGenAlgConfig
  (MinLength minL)
  (MaxLength maxL)
  (PopSize size)
  (CrossoverRate xRate)
  (MutationRate mRate)
  maxIt
  unfitness = if cond
          then Just config
          else Nothing
  where
    cond = (minL <= maxL) &&
           (minL > 0) &&
           (size > 0) &&
           (xRate >= 0) && (xRate <= 1) &&
           (mRate >= 0) && (mRate <= 1) &&
           (maxIt >= 0)

    config = GenAlgConfig
             (MinLength minL)
             (MaxLength maxL)
             (PopSize size)
             (CrossoverRate xRate)
             (MutationRate mRate)
             maxIt
             unfitness


-- | The constraint that the semantics of all instances need to adhere to. The
-- 'Show' constraint on the specific 'Chromosome' instance isn't strictly
-- necessarily but eases debugging
type GASemConstraint sem = (GenAlg sem, Show (Chromosome (Dom sem)))


-- | Type class representing a particular genetic algorithm formulation.
-- 'sem' represents the specific semantics which are conferred by an instance.
class GenAlg sem where
  -- | The type characterizing the problem domain
  type Dom sem :: *

  -- | Crossover operation. This always succeeds and is a design choice. This
  -- puts the burden on the instances to handle failure, if any, gracefully
  -- (perhaps by returning the original parents).
  crossover :: (GASemConstraint sem)
               => GenAlgConfig (Dom sem) sem
               -> (Chromosome (Dom sem))
               -> (Chromosome (Dom sem))
               -> (Rand StdGen) ((Chromosome (Dom sem)), (Chromosome (Dom sem)))

  -- | Mutation operation. This always succeeds and is a design choice. This
  -- puts the burden on the instances to handle failure, if any, gracefully
  -- (perhaps by returning the original chromosome).
  mutation :: (GASemConstraint sem)
              => GenAlgConfig (Dom sem) sem
              -> (Chromosome (Dom sem))
              -> (Rand StdGen) (Chromosome (Dom sem))

  -- | We should be able to generate a random chromosome
  randomChromosome :: (GASemConstraint sem)
                   => GenAlgConfig (Dom sem) sem
                   -> Rand StdGen (Chromosome (Dom sem))

  -- | Allow instances to override convergence criteria. If greater flexibility
  -- is required , this can be made part of 'GenAlgConfig' instead
  -- (eg. 'unfitnessScore')
  hasConverged :: (GASemConstraint sem)
                  => GenAlgConfig (Dom sem) sem
                  -> [(Chromosome (Dom sem))]
                  -> Bool
  hasConverged conf xs = Nothing `elem` (map (fitnessScore conf) xs)

  -- | Main entry point. Evolve for 'maxIterations' or till algorithm converges
  -- whichever occurs first. A 'Right' output (in the 'EitherT' monad)
  -- represents an output population which has converged. Instances can override
  -- this and 'evolve' to do more sophisticated things (eg. set up a simulated
  -- annealing like schedule wrt mutation/crossover rates.
  evolveForTime :: (GASemConstraint sem)
                => GenAlgConfig (Dom sem) sem
                -> EitherT [(Chromosome (Dom sem))] (Rand StdGen) [(Chromosome (Dom sem))]
  evolveForTime conf = EitherT $ do
    pop <- runEitherT $ initPop conf
    runEitherT $ evolve conf (conf ^.maxIterations) pop

  -- | Another entry point. Can allow you to continue from prior state.
  evolve :: (GASemConstraint sem)
         => GenAlgConfig (Dom sem) sem
         -> IterationsLeft
         -> Either [(Chromosome (Dom sem))] [(Chromosome (Dom sem))]
         -> EitherT [(Chromosome (Dom sem))] (Rand StdGen) [(Chromosome (Dom sem))]
  evolve _ 0 xs = EitherT $ return xs
  evolve conf n xs = case xs of
    Right(_) -> EitherT $ return xs
    Left(ys) -> EitherT $ do
      newPop <- evolutionStep conf ys
      runEitherT $ evolve conf (n-1) (passVerdict conf newPop)

----------------------------------------------
-- Internal helper functions (not exported) --
----------------------------------------------

-- | INTERNAL: initialize population
initPop :: (GASemConstraint sem)
           => GenAlgConfig (Dom sem) sem
           -> EitherT [(Chromosome (Dom sem))] (Rand StdGen) [(Chromosome (Dom sem))]
initPop conf = EitherT $ do
  pop <- runListT (popBySize (conf ^.populationSize))
  return $ (passVerdict conf pop)
  where
    popBySize (PopSize 0) = mzero
    popBySize (PopSize n) = (ListT $ fmap return (randomChromosome conf)) `mplus`
                  (popBySize (PopSize $ n-1))


-- | INTERNAL: pass judgment on current population wrt convergence
passVerdict :: (GASemConstraint sem)
               => GenAlgConfig (Dom sem) sem
               -> [(Chromosome (Dom sem))]
               -> Either [(Chromosome (Dom sem))] [(Chromosome (Dom sem))]
passVerdict conf pop = if hasConverged conf pop
                       then Right pop
                       else Left pop

-- | INTERNAL: compute fitness score from 'unfitnessScore'
fitnessScore :: (GASemConstraint sem)
                => GenAlgConfig (Dom sem) sem
                -> (Chromosome (Dom sem))
                -> Maybe Rational
fitnessScore conf x = case (conf ^.unfitnessScore) x of
  0 -> Nothing
  unfitness -> Just $ 1 / unfitness

-- | INTERNAL: take one evolutionary step
evolutionStep :: (GASemConstraint sem)
                 => GenAlgConfig (Dom sem) sem
                 -> [(Chromosome (Dom sem))]
                 -> (Rand StdGen) [(Chromosome (Dom sem))]
evolutionStep _ [] = return []
evolutionStep conf pop = do
  samples <- sequence . (take (sampleSize)) . repeat . fromList . weightedPop conf $ pop
  let parents = pairs $ samples
  coupledChildren <- sequence $ map (\(x, y) -> crossover conf x y) parents
  let nextGen = concatMap (\(x, y) -> [x, y]) coupledChildren
  newPop <- sequence $ map (mutation conf) nextGen
  return $ take (popSize $ conf ^.populationSize) newPop
  where
    sampleSize = 2 * (ceiling ((toInteger . popSize $ conf ^.populationSize) % 2))

    popSize (PopSize size) = size

    pairs :: [t] -> [(t, t)]
    pairs [] = []
    pairs [_] = []
    pairs (x:y:xs) = (x, y) : pairs xs

    weightedPop :: (GASemConstraint sem)
                 => GenAlgConfig (Dom sem) sem
                 -> [(Chromosome (Dom sem))]
                 -> [(Chromosome (Dom sem), Rational)]
    weightedPop c xs = let
      fitness = map (fitnessScore c) xs
      in
       case sequence fitness of
         Nothing -> zip
                    (zipWith (!!) (repeat xs) (Nothing `elemIndices` fitness))
                    (repeat 1)
         Just _ -> zip xs (map fromJust fitness)

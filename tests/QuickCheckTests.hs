{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module QuickCheckTests where

import Algorithm.GenAlg hiding (mkGenAlgConfig)
import Algorithm.GenAlg.Dist
import Algorithm.Domain.SimpleArith
import Control.Monad.Random
import Data.List
import Data.Maybe
import Data.Ratio

import Control.Lens

import Test.QuickCheck.Property as P

import qualified Test.Framework as TF (Test)
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2 (testProperty)


-- | Naive semantics to perform genetic algorithm in the 'Chromosome Digits'
-- domain. These semantics are still configuration dependent, but pin down the
-- (configuration dependent) semantics of the genetic operators ('mutation', and
-- 'crossover'), 'unfitnessScore' and the distribution on chromosomes
-- ('randomChromosome').
data NaiveDigitsStrategy

instance GenAlg NaiveDigitsStrategy where
  type Dom NaiveDigitsStrategy = Digits
  mutation = mutationOp
  crossover = crossoverOp
  randomChromosome = randomChromosomeOp

defaultConfig :: Maybe (GenAlgConfig (Dom NaiveDigitsStrategy) NaiveDigitsStrategy)
defaultConfig = mkGenAlgConfig
                (MinLength 5)
                (MaxLength 5)
                (PopSize 100)
                (CrossoverRate 0.7)
                (MutationRate 0.1) -- works better for smaller number of iterations
                300
                (const $ 1%1) -- override this

type ConfigType = GenAlgConfig (Dom NaiveDigitsStrategy) NaiveDigitsStrategy
setTarget :: ConfigType
          -> Rational
          -> ConfigType
setTarget conf target = (conf & unfitnessScore .~ (\xs -> abs(exprVal(xs) - target)))


prop_bernoulli_can_approximate_fair_coin :: Int -> P.Result
prop_bernoulli_can_approximate_fair_coin seed = let
  (coins, _) = runRand (sequence . (take samples) . repeat . bernoulli $ p) (mkStdGen seed)
  in
   MkResult
   (Just (check coins))
   True
   ("Actual p = " ++ show p ++
    "\n" ++ "Sampled p = " ++ show (count coins))
   False
   True
   []
   []
  where
    p = 0.5
    count xs = fromRational . toRational $ (length $ True `elemIndices` xs) % samples
    check xs = abs(p - (count xs)) < 0.1
    samples = 100

prop_can_create_diverse_pool :: Int -> Bool
prop_can_create_diverse_pool seed =
  case ((runRand . runEitherT) (evolveForTime (conf :: ConfigType))
        (mkStdGen seed)) of
    (Left xs, _) -> check xs
    (Right xs, _) -> check xs
  where
    check xs = (length . group . sort . map exprVal) xs > 1
    conf = (fromJust defaultConfig) & maxIterations .~ 0

prop_GA_search_converges_quickly :: Int -> P.Result
prop_GA_search_converges_quickly seed = case defaultConfig of
  Nothing -> MkResult (Just False) True ("Config failed!") False True [] []
  Just conf ->
    case ((runRand . runEitherT) (evolveForTime (setTarget conf target)) (mkStdGen seed)) of
      (Left xs, _) -> check xs False
      (Right xs, _) -> check xs True
  where
    target = 23 % 1
    check xs judgment = MkResult
                        (Just judgment)
                        True
                        ("Target = " ++ show target ++ "\n" ++
                         "Attempted solutions = " ++
                         (show $ attempted xs))
                        False
                        True
                        []
                        []
    attempted :: [Chromosome Digits] -> [Double]
    attempted xs = map (fromRational . exprVal) xs


tests :: TF.Test
tests  = $(testGroupGenerator)

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Algorithm.Domain.SimpleArith where

import Algorithm.GenAlg hiding (mkGenAlgConfig)
import qualified Algorithm.GenAlg as GA (mkGenAlgConfig)
import Algorithm.GenAlg.Dist

import Prelude hiding (head, undefined)
import Control.Monad.Random
import Control.Lens
import Data.Data
import Data.Data.Lens (uniplate)
import Safe

import Test.QuickCheck

data Digits = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
            deriving (Bounded, Enum, Eq, Ord, Read, Show, Typeable, Data)

-- | Generate random digits
instance Random Digits where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g


-- | Generate random Digits for testing with QuickCheck
instance Arbitrary Digits where
  arbitrary = arbitraryBoundedRandom
  shrink = shrinkNothing

-- | The AST of our toy arithmetic language. Since operation parsing is from
-- left to right, what this means is that our AST is quite
-- restricted. Specifically, one branch is always atomic. eg.
--     6+5*4/2+1 = ((((6 + 5) * 4) / 2) + 1)
-- In our toy AST this will read as:
-- IncBy(1, DivBy(2, MulBy(4, IncBy(5, 6))))
data instance Chromosome Digits = Lit Digits
                                | IncBy (Digits) (Chromosome Digits)
                                | MulBy (Digits) (Chromosome Digits)
                                | DecBy (Digits) (Chromosome Digits)
                                | DivBy (Digits) (Chromosome Digits)
                                  deriving (Eq, Show, Data, Typeable)

-- | Eta-equivalence for 'Chromosome Digits' data constructors. This works
-- because:
--
-- * 'Chromosome Digits' derives Eq
-- * Two 'Chromosome Digits' are only ever equal if every node in their AST is
-- equal to the other
instance Eq (Digits -> (Chromosome Digits) -> (Chromosome Digits)) where
  f == g = (f One (Lit One)) == (g One (Lit One))

-- | Eq instance for Lit constructor
instance Eq (Digits -> Chromosome Digits) where
  f == g = f One == g One

-- | Plated instance to allow us to perform generic operations
instance Plated (Chromosome Digits) where
  plate = uniplate

-- | Generate a valid partial chromosome. In our particular case the validity of
-- a chromosome is a recursive property i.e., a chromosome is valid if its head
-- is valid and its tail is valid.
--
-- TODO/HMM:
--
-- * add a Random (Chromosome Digits -> Chromosome Digits) instance
-- * include case for Lit constructor (w/ PolyKinds perhaps?)
type PartialOp = ((Digits -> Chromosome Digits -> Chromosome Digits), Digits)
randomPair :: Maybe (PartialOp -- ^ Starting pair
                     , Rate)
           -> (Rand StdGen) PartialOp
randomPair startState = case startState of
  Nothing -> do
    digitOp <- uniform digitOps
    digit <- validLeft digitOp
    return $ (digitOp, digit)
  Just ((digitOp, digit), rate) -> do
    replace <- bernoulli rate
    potentialOp <- uniformButNot digitOp digitOps
    let newOp = if replace
                then potentialOp
                else digitOp
    newDigit <- if replace
                then validLeft newOp
                else return digit
    return $ (newOp, newDigit)

  where
    digitOps = [IncBy, MulBy, DecBy, DivBy]
    validLeft digitOp = if digitOp == DivBy
                        then randomButNot Zero
                        else liftRand random

-- | Calculate the size of a chromosome.
--
-- NOTE: This limits the maximum size of a Chromosome, since it internally uses
-- the list 'length' function
size :: (Chromosome Digits)
     -> Length
size = toInteger . length . universe


-- | An interpretation of the toy arithmetic language
exprVal :: Chromosome Digits -> Rational
exprVal e = case e of
  Lit x -> litVal x
  IncBy x y -> (litVal x) + (exprVal y)
  MulBy x y -> (litVal x) * (exprVal y)
  DecBy x y -> (exprVal y) - (litVal x)
  DivBy x y -> (exprVal y) / (litVal x)
  where
    litVal = toRational . fromEnum

-- | Modified genetic algorithm configuration generator. We:
--
-- * cap the maximum chromosome length. This is done because current
-- chromosome representation doesn't scale well to larger sizes. The current cap
-- of 30 isn't tight. An absolute upper bound on the maximum permissible
-- chromosome length is given by:
--
-- > maxBound :: Int
mkGenAlgConfig :: MinLength
               -> MaxLength
               -> PopSize
               -> CrossoverRate
               -> MutationRate
               -> MaxIterations
               -> (Chromosome repr -> Rational)
               -> Maybe (GenAlgConfig repr range)
mkGenAlgConfig
  minl@(MinLength _)
  maxl@(MaxLength maxL)
  popsize@(PopSize _)
  xrate@(CrossoverRate _)
  mrate@(MutationRate _)
  maxIt
  unfitness = let
  config = GA.mkGenAlgConfig minl maxl popsize xrate mrate maxIt unfitness
  in
        if cond
        then config
        else Nothing
  where
        cond = (maxL <= 30)

--------------
-- mutation --
--------------
doMutation :: MutationRate
           -> (Chromosome Digits)
           -> (Rand StdGen) (Chromosome Digits)
-- ^ Main mutation worker.
-- TODO:
--
-- * Use a generics library (lens plated?) to get rid of repetition
doMutation (MutationRate rate) (Lit digit) = do
  doReplace <- bernoulli rate
  replacementDigit <- randomButNot digit
  let newDigit = if doReplace
                 then replacementDigit
                 else digit
  return $ Lit newDigit
doMutation rate (IncBy digit expr) = mutationWorker rate IncBy digit expr
doMutation rate (MulBy digit expr) = mutationWorker rate MulBy digit expr
doMutation rate (DecBy digit expr) = mutationWorker rate DecBy digit expr
doMutation rate (DivBy digit expr) = mutationWorker rate DivBy digit expr

mutationWorker :: MutationRate
               -> (Digits -> Chromosome Digits -> Chromosome Digits)
               -> Digits
               -> Chromosome Digits
               -> (Rand StdGen) (Chromosome Digits)
mutationWorker mrate@(MutationRate rate) fun lit expr = do
  (newFun, newLit) <- randomPair $ Just ((fun, lit), rate)
  newExpr <- doMutation mrate expr
  return $ newFun newLit newExpr

mutationOp :: GenAlgConfig Digits a
              -> (Chromosome Digits)
              -> (Rand StdGen) (Chromosome Digits)
mutationOp conf = doMutation (conf ^.mutRate)

---------------
-- crossover --
---------------
type PrefixLength = Length
doCrossover :: GenAlgConfig Digits a
            -> (Chromosome Digits)
            -> (Chromosome Digits)
            -> (Rand StdGen) ((Chromosome Digits), (Chromosome Digits))
doCrossover conf x1 x2 = do
  shouldCrossover <- crossOrNot (conf ^.crossRate)
  if  shouldCrossover
  then do
    -- pick a random points in [0, n1) and [0, n2) excluding (0,0)
    (p1, p2) <- crossoverPoints
                (conf^.minChromosomeLength) (conf^.maxChromosomeLength)
                x1 x2
    let offspring = do
                  -- use splitSuffix to extract suffixes
                  s1 <- splitSuffix x1 p1
                  s2 <- splitSuffix x2 p2
                  -- perform crossover
                  y1 <- patchSuffix x1 p1 s2 -- p1 + l2 - p2
                  y2 <- patchSuffix x2 p2 s1 -- p2 + l1 - p1
                  return (y1, y2)
    case offspring of
         Nothing -> return(x1, x2) -- return original in case of failure
         Just(y1, y2) -> return (y1, y2)
  else return (x1, x2)

  where
    crossOrNot (CrossoverRate rate) = bernoulli rate

    crossoverPoints :: MinLength
                    -> MaxLength
                    -> (Chromosome Digits)
                    -> (Chromosome Digits)
                    -> Rand StdGen (Length, Length)
    crossoverPoints (MinLength minL) (MaxLength maxL) x y = case (size x, size y) of
      (1, 1) -> return (0,0)
      (l1, l2) -> uniformButNot (0,0)
                  [(p1, p2) | p1 <- [0..(l1-1)], p2 <- [0..(l2-1)],
                            inRange minL maxL (l2 + p1 - p2),
                            inRange minL maxL (l1 - p1 + p2)]

    inRange minL maxL l = (minL <= l) && (l <= maxL)

    -- | Also limits maximum length of chromosome, since internally uses
    -- Int. Note, that since this representation isn't meant to be very scalable
    -- this is not likely to be the limiting factor
    splitSuffix :: (Chromosome Digits) -- ^ Original chromosome
                -> PrefixLength -- ^ Discard prefix from 0 to here
                -> Maybe (Chromosome Digits)
    splitSuffix xs n = atMay (universe xs) (fromInteger n)

    patchSuffix :: (Chromosome Digits) -- ^ Original Chromosome
                -> PrefixLength -- ^ Keep prefix from 0 to here
                -> (Chromosome Digits) -- ^ Suffix to be patched in at position n
                -> Maybe (Chromosome Digits)
    patchSuffix xs n ys = case xs of
      Lit _ -> case n of
        0 -> Just ys
        _ -> Nothing
      IncBy z zs -> (patchSuffix zs (n-1) ys) >>= return . IncBy z
      MulBy z zs -> (patchSuffix zs (n-1) ys) >>= return . MulBy z
      DecBy z zs -> (patchSuffix zs (n-1) ys) >>= return . DecBy z
      DivBy z zs -> (patchSuffix zs (n-1) ys) >>= return . DivBy z

crossoverOp :: GenAlgConfig Digits a
            -> (Chromosome Digits)
            -> (Chromosome Digits)
            -> (Rand StdGen) ((Chromosome Digits), (Chromosome Digits))
crossoverOp conf = doCrossover conf


----------------------
-- randomChromosome --
----------------------
getRandomChromosome :: MinLength
                    -> MaxLength
                    -> (Rand StdGen) (Chromosome Digits)
getRandomChromosome (MinLength l1) (MaxLength l2) =
  (liftRand . randomR $ (l1, l2)) >>= randomChromosomeOfLength
  where
    randomChromosomeOfLength :: Length
                             -> (Rand StdGen) (Chromosome Digits)
    randomChromosomeOfLength 1 = do
      digit <- liftRand random
      return $ Lit digit
    randomChromosomeOfLength n = do
      (digitOp, digit) <- randomPair Nothing
      rest <- randomChromosomeOfLength (n-1)
      return $ digitOp digit rest

randomChromosomeOp :: GenAlgConfig Digits a
                   -> (Rand StdGen) (Chromosome Digits)
randomChromosomeOp conf = (getRandomChromosome minLength maxLength)
  where
        minLength = (conf ^.minChromosomeLength)
        maxLength = (conf ^.maxChromosomeLength)

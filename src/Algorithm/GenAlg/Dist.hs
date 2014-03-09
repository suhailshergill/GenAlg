{-|
Module: Algorithm.GenAlg.Dist

Some utilities which allow us to sample in particular ways
-}
module Algorithm.GenAlg.Dist (
  uniformButNot
  , randomButNot
  , bernoulli
  ) where

import Control.Monad.Random


uniformButNot :: (Eq a, MonadRandom m)
              => a
              -> [a]
              -> m a
uniformButNot x xs = uniform matches where
  matches = [y | y <- xs, y /= x]


randomButNot :: (Bounded a, Enum a, Eq a, MonadRandom m)
             => a
             -> m a
randomButNot x = uniform range where
  range = [y | y <- enumFromTo minBound maxBound, y /= x]


bernoulli :: (RandomGen g)
          => Double -- ^ Success probability
          -> (Rand g) Bool
bernoulli p = do
  val <- liftRand $ randomR (0.0 :: Double, 1.0 :: Double)
  if val <= p
    then return $ True
    else return $ False

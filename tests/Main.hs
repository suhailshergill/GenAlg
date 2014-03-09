module Main where

import Test.Framework as TF (defaultMainWithOpts)
import Test.Framework.Runners.Options
import Test.Framework.Options
import Test.Framework.Seed
import qualified QuickCheckTests as QCT (tests)

main :: IO ()
main  = defaultMainWithOpts [QCT.tests] $
        RunnerOptions
        (Just 1)
        (Just (TestOptions
              (Just (FixedSeed 1))
              (Just 10)
              (Just 5)
              Nothing
              Nothing
              (Just (Just 10000000))))
        Nothing
        Nothing
        Nothing
        (Just ColorAlways)
        (Just False)
        Nothing

{-# LANGUAGE GADTs, StandaloneDeriving, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module GA.Problem where

import Data.Ratio


data Atoms = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
                   deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- ok so what're the operations we want to be able to support?
-- 1] convert Atoms to genes
-- 2] define AtomOps over genes
-- 3] be able to randomly select Atoms and AtomOps
-- 4] be able to walk over an AtomOps AST
-- 5] be able to interpret AtomOps AST as a Rational, allowing one to define
--    fitness function

-- let's look at some example use cases.
-- Zero, One, Two, ..., Nine
-- 6+5*4/2+1 = (((6 + 5) * 4) / 2) + 1
-- Add(Div(Mul(Add(6, 5),4), 2), 1)
-- IncBy(1, DivBy(2, MulBy(4, IncBy(5, 6))))

-- |Initial encoding
data Expr a where
  Lit :: Atoms -> Expr a
  IncBy :: Expr a -> Expr a -> Expr a
  MulBy :: Expr a -> Expr a -> Expr a
  DecBy :: Expr a -> Expr a -> Expr a
  DivBy :: Expr a -> Expr a -> Expr a

-- ok so let's go with this initial coding. we've got all we need is the ability
-- to walk through the structure. for now settle with pattern-matching?

-- |Final encoding
class ExprSym dom where
  lit :: Atoms -> dom
  incBy :: dom -> dom -> dom
  mulBy :: dom -> dom -> dom
  decBy :: dom -> dom -> dom
  divBy :: dom -> dom -> dom

instance  ExprSym (Ratio Integer) where
  lit = (% 1) . toInteger . fromEnum
  incBy = flip (+)
  mulBy = flip (*)
  decBy = flip (-)
  divBy = flip (/)
-- ok, but what about serialize/deserialization?

-- data AtomOps a where
--   Lit :: Atoms -> AtomOps Atoms

--   Add :: AtomOps Atoms -> AtomOps Atoms -> AtomOps Atoms
--   Mul :: AtomOps Atoms -> AtomOps Atoms -> AtomOps Atoms
--   Sub :: AtomOps Atoms -> AtomOps Atoms -> AtomOps Atoms
--   Div :: AtomOps Atoms -> AtomOps Atoms -> AtomOps Atoms
-- deriving instance Show (AtomOps a)
-- deriving instance Eq (AtomOps a)
-- deriving instance (a ~ Atoms) => Read (AtomOps a)

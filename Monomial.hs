{-# LANGUAGE FlexibleInstances #-}

module Monomial
--( Vars(X,Y,Z)
--,
( Monomial(M)
, inject
, toList
, fromList
, exponent
, variables
, isDivisibleBy
, div
, lcm
, complement
, HasDegree
, degree
, Enumerable
, enumerate
, Lex
) where

import Data.Maybe
import qualified Data.Map as Map
import Prelude hiding (lcm, div, lex, exponent)

--data Vars = X | Y | Z deriving (Eq, Ord, Show)

class Ord a => Enumerable a where
    enumerate :: [a]

--instance Enumerable Vars where enumerate = [X, Y, Z]

newtype Monomial v o = M (Map.Map v Int) deriving Eq

inject :: Eq v => v -> Monomial v o
inject x = M $ Map.singleton x 1

toList :: Ord v => Monomial v o -> [(v, Int)]
toList (M m) = [ p | p@(x, n) <- Map.toList m, n /= 0 ]

fromList :: Ord v => [(v, Int)] -> Monomial v o
fromList xs = M $ Map.fromList [ p | p@(x, n) <- xs, n /= 0 ]

exponent :: Ord v => v -> Monomial v o -> Int
exponent x (M m) = fromMaybe 0 (Map.lookup x m)

variables :: Ord v => Monomial v o -> [v]
variables = map fst . toList

instance (Ord v, Show v) => Show (Monomial v o) where
    show m
        | null support
        = "1"
        | otherwise
        = concat [ show x ++ suffix
                 | (x, n) <- support
                 , let suffix = if n == 1
                                then ""
                                else "^" ++ show n
                 ]
        where
          support = toList m

instance Ord v => Monoid (Monomial v o) where
    mempty = M Map.empty
    M a `mappend` M b = M $ Map.unionWith (+) a b

class HasDegree a where
  degree :: a -> Int

instance Ord v => HasDegree (Monomial v o) where
    degree (M m) = Map.fold (+) 0 m

isDivisibleBy :: Ord v => Monomial v o -> Monomial v o -> Bool
isDivisibleBy (M a) (M b) = Map.isSubmapOfBy (<=) b a

div :: Ord v => Monomial v o -> Monomial v o -> Monomial v o
div (M a) (M b) = M $ Map.differenceWith sub a b
    where
      sub x y | x > y     = Just (x - y)
              | otherwise = Nothing

lcm :: Ord v => Monomial v o -> Monomial v o -> Monomial v o
lcm (M a) (M b) = M $ Map.unionWith max a b

complement :: Ord v => Monomial v o -> Monomial v o -> Monomial v o
complement m n = lcm m n `div` m

data Lex         -- Lexicographic ordering
data RevLex      -- Reverse lexicographic ordering
data DegLex      -- Degree lexicographic ordering
data DegRevLex   -- Reverse degree lexicographic ordering

lex' :: (Ord v, Show v) => Monomial v o -> Monomial v o -> [v] -> Bool
lex' a b []     = True
lex' a b (x:xs) = exponent x a <= exponent x b
                  && (exponent x a /= exponent x b || lex' a b xs)

lex, revlex, deglex, degrevlex :: (Enumerable v, Show v)
                               => Monomial v o -> Monomial v o -> Bool
lex       a b = lex' a b enumerate
revlex    a b = lex' a b (reverse enumerate)
deglex    a b = degree a <= degree b
                && (degree a /= degree b || a `lex` b)
degrevlex a b = degree a <= degree b
                && (degree a /= degree b || b `revlex` a)

instance (Show v, Enumerable v) => Ord (Monomial v Lex) where
     (<=) = lex
instance (Show v, Enumerable v) => Ord (Monomial v RevLex) where
    (<=) = revlex
instance (Show v, Enumerable v) => Ord (Monomial v DegLex) where
    (<=) = deglex
instance (Show v, Enumerable v) => Ord (Monomial v DegRevLex) where
        (<=) = degrevlex

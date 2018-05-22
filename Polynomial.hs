{-# LANGUAGE FlexibleContexts #-}

module Polynomial where

import Monomial
import Data.List(intersect)

data Term r v o = T r (Monomial v o) deriving (Eq, Show)


instance (Num r, Ord v) => Monoid (Term r v o) where
    mempty = T 1 mempty
    T a m `mappend` T b n = T (a * b) (m `mappend` n)


newtype Polynomial r v o = P [Term r v o] deriving Eq


lm :: Polynomial r v o -> Monomial v o
lm (P ((T _ m):_)) = m
lm (P [])          = error "lm: zero polynomial"


(*^) :: (Num r, Ord v) => Term r v o -> Polynomial r v o -> Polynomial r v o
u *^ P vs = P [ u `mappend` v | v <- vs ]


instance Ord v => HasDegree (Polynomial r v o) where
    degree (P []) = -1
    degree (P ts) = maximum [ degree m | T _ m <- ts ]


instance (Eq r, Show r, Num r, Ord v, Show v) => Show (Polynomial r v o) where
    show (P [])     = "0"
    show (P (t:ts)) = showHead t ++ showTail ts
        where
          showHead (T c m) = prefix ++ show m
              where
                prefix = case c of
                           1  -> ""
                           -1 -> "-"
                           _  -> show c
          showTerm (T c m) = prefix ++ show m
              where
                prefix = case signum c of
                           1  -> '+':a
                           -1 -> '-':a
                           _  -> "(" ++ show c ++ ")"
                a = if abs c == 1 then "" else show (abs c)
          showTail = concatMap showTerm


constant :: (Eq r, Num r, Ord v) => r -> Polynomial r v o
constant 0 = P []
constant c = P [T c mempty]


instance (Eq r, Num r, Ord v, Show v, Ord (Monomial v o))
    => Num (Polynomial r v o) where
    f@(P (u@(T a m):us)) + g@(P (v@(T b n):vs))
        | m == n && a + b /= 0
        = let P ws = P us + P vs in P $ T (a + b) m:ws
        | m == n && a + b == 0
        = P us + P vs
        | m < n
        = let P ws = f + P vs in P $ v:ws
        | otherwise
        = let P ws = P us + g in P $ u:ws
    f + P [] = f
    P [] + g = g

    P (u:us) * P (v:vs)
        = let P ws = P us * P vs + u *^ P vs + v *^ P us
          in P $ (u `mappend` v):ws
    _ * P [] = P []
    P [] * _ = P []

    negate (P ts) = P $ [ T (negate a) m | T a m <- ts ]
    -- Inclusion of 'abs' and 'signum' into 'Num' was a stupid idea.
    abs _ = error "abs is undefined for polynomials"
    signum _ = error "signum is undefined for polynomials"
    fromInteger = constant . fromInteger


variable :: (Num r, Eq v) => v -> Polynomial r v o
variable x = P [T 1 (inject x)]


spoly :: (Eq r, Fractional r, Ord v, Show v, Ord (Monomial v o))
      => Polynomial r v o -> Polynomial r v o -> Polynomial r v o
spoly f@(P (u@(T a m):us)) g@(P (v@(T b n):vs)) = n' *^ f - m' *^ g
    where
      n' = T 1       (complement m n)
      m' = T (a / b) (complement n m)


normal_form :: (Eq r, Fractional r, Ord v, Show v, Ord (Monomial v o))
   => Polynomial r v o -> [Polynomial r v o] -> Polynomial r v o
normal_form f s = go f
   where
     go h | h == 0      = 0
          | []    <- s' = h
          | (g:_) <- s' = go (spoly h g)
          where
            s' = [g | g <- s, lm h `isDivisibleBy` lm g]


groebner_not_optimized :: (Eq r, Fractional r, Ord v, Show v, Ord (Monomial v o))
                       => [Polynomial r v o] -> [Polynomial r v o]
groebner_not_optimized i = go i ps
    where
      ps = [(f, g) | f <- i, g <- i, f /= g]
      go s [] = s
      go s ps@((f, g):ps')
          | h == 0    = go s ps'
          | otherwise = go (h:s) (ps' ++ [(h, f) | f <- s])
          where
            h = normal_form (spoly f g) s

groebner_optimized :: (Eq r, Fractional r, Ord v, Show v, Ord (Monomial v o))
                   => [Polynomial r v o] -> [Polynomial r v o]
groebner_optimized i = go i ps
    where
      pc f g = null (variables (lm f) `intersect` variables (lm g))
      ps = [(f, g) | f <- i, g <- i, f /= g, not (pc f g)]
      go s [] = s
      go s ps@((f, g):ps')
          | h == 0    = go s ps'
          | otherwise = go (h:s) (ps' ++ [(h, f) | f <- s, not (pc h f)])
          where
            h = normal_form (spoly f g) s

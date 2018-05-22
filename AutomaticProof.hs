{-# LANGUAGE FlexibleContexts, TypeOperators, TemplateHaskell #-}

import Monomial
import Polynomial
import Variable
import Language.Haskell.TH


$(defineVariables ["X", "Y", "Z"])

ideal :: Ord (Monomial (X :<: Y) o) => [Polynomial Rational (X :<: Y) o]
ideal = [x ^ 10 + x ^ 9 * y ^ 2, y ^ 8 - x ^ 2 * y ^ 7]

basis :: Ord (Monomial (X :<: Y) o) => [Polynomial Rational (X :<: Y) o]
basis = groebner ideal

isIn ::(Eq r, Fractional r, Ord v, Show v, Ord (Monomial v o))
     =>  Polynomial r v o -> [Polynomial r v o] -> Bool
isIn p s = if normal_form p s == (P []) then True else False

evaluateLex :: (Polynomial Rational (X :<: Y) Lex )-> [Polynomial Rational (X :<: Y) Lex] -> Bool
evaluateLex p s = isIn p s

main = putStr . unlines $
       [ "Ideal:"
       , ln (ideal :: [Polynomial Rational (X :<: Y) Lex])
       , "Lex basis:"
       , ln (basis :: [Polynomial Rational (X :<: Y) Lex])
       , "Is x ^ 10 + x ^ 9 * y ^ 2 ?"
       , show (evaluateLex (x ^ 10 + x ^ 9 * y ^ 2) basis)
       , "Is x ^ 11 + x ^ 9 * y ^ 2 ?"
       , show (evaluateLex (x ^ 11 + x ^ 9 * y ^ 2) basis)
       ]
    where
      ln :: Ord (Monomial (X :<: Y) o)
          => [Polynomial Rational (X :<: Y) o] -> String
      ln = unlines . map (("  " ++) . show)

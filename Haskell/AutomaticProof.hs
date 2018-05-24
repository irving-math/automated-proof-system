{-# LANGUAGE FlexibleContexts, TypeOperators, TemplateHaskell #-}

module AutomaticProof where

import Monomial
import Polynomial
import Variable
import Language.Haskell.TH


$(defineVariables ["X", "Y", "Z", "X1", "X2", "X3", "Y1", "Y2", "Y3", "Z1", "Z2", "Z3"])

-- Ideal generico
ideal_generic :: [Polynomial Rational (X :<: Y) Lex]
ideal_generic = [x ^ 10 + x ^ 9 * y ^ 2, y ^ 8 - x ^ 2 * y ^ 7]

isIn ::(Eq r, Fractional r, Ord v, Show v, Ord (Monomial v Lex))
     =>  Polynomial r v Lex -> [Polynomial r v Lex] -> Bool
isIn p s = if normal_form p s == (P []) then True else False

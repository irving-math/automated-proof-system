{-# LANGUAGE FlexibleContexts, TypeOperators, TemplateHaskell #-}

module AutomaticProof where

import Monomial
import Polynomial
import Variable
import Language.Haskell.TH


$(defineVariables ["X", "Y", "Z"])

-- Ideal generico
ideal :: [Polynomial Rational (X :<: Y) Lex]
ideal = [x ^ 10 + x ^ 9 * y ^ 2, y ^ 8 - x ^ 2 * y ^ 7]

basis :: [Polynomial Rational (X :<: Y) Lex] -> [Polynomial Rational (X :<: Y) Lex]
basis ideal' = groebner ideal'

isIn ::(Eq r, Fractional r, Ord v, Show v, Ord (Monomial v Lex))
     =>  Polynomial r v Lex -> [Polynomial r v Lex] -> Bool
isIn p s = if normal_form p s == (P []) then True else False

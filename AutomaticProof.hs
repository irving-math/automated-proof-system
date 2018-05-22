{-# LANGUAGE FlexibleContexts, TypeOperators, TemplateHaskell #-}

import Monomial
import Polynomial
import Variable
import Language.Haskell.TH


$(defineVariables ["X", "Y", "Z"])

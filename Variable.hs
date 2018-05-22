{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}
module Variable where

import Monomial
import Polynomial

import Control.Arrow
import Control.Monad
import Data.Char
import Language.Haskell.TH


data a :<: b = Inl a | Inr b deriving (Eq, Ord)
infixr 6 :<:

instance (Show a, Show b) => Show (a :<: b) where
    show (Inl x) = show x
    show (Inr x) = show x

instance (Enumerable a, Enumerable b) => Enumerable (a :<: b) where
    enumerate = map Inl enumerate ++ map Inr enumerate

class Sub a b where
    inj :: a -> b

instance Sub a a where
    inj = id

instance Sub a (a :<: b) where
    inj = Inl

instance Sub a c => Sub a (b :<: c) where
    inj = Inr . inj


var :: (Sub v w, Ord (Monomial w o), Num r, Eq w) => v -> Polynomial r w o
var = variable . inj

-- Macrology

-- $(defineData (mkName "X")) produces the following data type
-- declaration:
--     data X = X deriving (Eq, Ord, Show)
defineData :: Name -> Q Dec
defineData name
    = dataD (cxt []) name [] Nothing [normalC name []] (mapM conT [''Eq, ''Ord, ''Show])

-- $(defineInst (mkName "X")) produces the following instance
-- declaration:
--     instance Enumerable X where { enumerate = [X] }
defineInst :: Name -> Q Dec
defineInst name
    = instanceD (cxt []) (appT (conT ''Enumerable) (conT name))
      [funD 'enumerate [clause [] (normalB (listE [conE name])) []]]

-- $(defineType (mkName "X")) produces the following type:
--     forall o r w. (Sub X w, Ord (Monomial w o), Num r, Eq w) =>
--     Polynomial r w o
defineType :: Name -> Q Type
defineType name
    = forallT [o_tv, r_tv, w_tv]
      (cxt [ classP ''Sub [conT name, w]
           , classP ''Ord [appManyT (conT ''Monomial) [w, o]]
           , classP ''Num [r]
           , classP ''Eq  [w]
           ])
      (appManyT (conT ''Polynomial) [r, w, o])
    where
      mkTyVar   = (PlainTV &&& varT) . mkName
      (o_tv, o) = mkTyVar "o"
      (r_tv, r) = mkTyVar "r"
      (w_tv, w) = mkTyVar "w"
      appManyT  = foldl appT

-- $(defineSig (mkName "x") (mkName "X")) produces the following type
-- signature:
--     x :: forall o r w. (Sub X w, Ord (Monomial w o), Num r, Eq w)
--       => Polynomial r w o
defineSig :: Name -> Name -> Q Dec
defineSig var_name ty_name
    = sigD var_name (defineType ty_name)

-- $(defineVal (mkName "x") (mkName "X")) produces the following value
-- definition:
--     x = var X
defineVal :: Name -> Name -> Q Dec
defineVal var_name con_name
    = valD (varP var_name) (normalB (appE (varE 'var) (conE con_name))) []

-- $(defineVariable "Foo") combines the above: it defines a data type
-- with a unique constructor Foo, makes it an instance of Enumerable,
-- and defines a polymorphic value foo as var Foo.
defineVariable :: String -> Q [Dec]
defineVariable str
    = sequence [ defineData name
               , defineInst name
               , defineSig var_name name
               , defineVal var_name name
               ]
    where
      name     = mkName (capitalize   str)
      var_name = mkName (uncapitalize str)
      capitalize   []     = []
      capitalize   (c:cs) = toUpper c : cs
      uncapitalize []     = []
      uncapitalize (c:cs) = toLower c : cs

defineVariables :: [String] -> Q [Dec]
defineVariables = concatMapM defineVariable

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

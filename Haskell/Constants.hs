
import AutomaticProof
import Monomial
import Polynomial
import Variable

--Primer problema
ideal_punto_2_2 :: [Polynomial Rational (X :<: Y) Lex]
ideal_punto_2_2 = [6*x-6*y,x-2]

--Paralelas en un cuadrado
ideal_paralelas :: [Polynomial Rational (X1 :<: X2 :<: X3 :<: Y1 :<: Y2 :<: Y3 :<: Z1 :<: Z2 :<: Z3) Lex]
ideal_paralelas = [y1*y3+x1*x3, y3*y2-y3^2+x2*x3-x3^2, z2*z3*x3*y3-z2*x3-z3*y3+1]

poli_paralelas :: Polynomial Rational (X1 :<: X2 :<: X3 :<: Y1 :<: Y2 :<: Y3 :<: Z1 :<: Z2 :<: Z3) Lex
poli_paralelas = y1*x2-y1*x3-x1*y2+x1*y3

module Peano() where

import Logic
import PrettyPrint
import Term
import Type

a1 =
  let x = mkVar "x" i
      s = mkCon "S" (func i i)
      zero = mkCon "0" i in
  forall $ mkLam x $ neg $ mkEq (mkApp s x) zero

a2 =
  let x = mkVar "x" i
      y = mkVar "y" i
      s = mkCon "S" (func i i) in
   forall $ mkLam x $ forall $ mkLam y $ (mkEq (mkApp s x) (mkApp s y)) --> (mkEq x y)

a3 =
  let p = mkVar "P" (func i o)
      x = mkVar "x" i
      s = mkCon "S" (func i i)
      zero = mkCon "0" i in
   forall $
   mkLam p $
   ((mkApp p zero) /\ (forall $ mkLam x $ (mkApp p x) --> (mkApp p (mkApp s x))))
   --> (forall $ mkLam x $ mkApp p x)

pa = [a1, a2, a3]

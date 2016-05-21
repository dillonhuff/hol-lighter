module Peano(a1, a2, a3, pa, nat) where

import Logic
import PrettyPrint
import Term
import Type

nat = tp "nat"

a1 =
  let x = mkVar "x" nat
      s = mkCon "S" (func nat nat)
      zero = mkCon "0" nat in
  forall $ mkLam x $ neg $ mkEq (mkApp s x) zero

a2 =
  let x = mkVar "x" nat
      y = mkVar "y" nat
      s = mkCon "S" (func nat nat) in
   forall $ mkLam x $ forall $ mkLam y $ (mkEq (mkApp s x) (mkApp s y)) --> (mkEq x y)

a3 =
  let p = mkVar "P" (func nat bool)
      x = mkVar "x" nat
      s = mkCon "S" (func nat nat)
      zero = mkCon "0" nat in
   forall $
   mkLam p $
   ((mkApp p zero) /\ (forall $ mkLam x $ (mkApp p x) --> (mkApp p (mkApp s x))))
   --> (forall $ mkLam x $ mkApp p x)

pa = [a1, a2, a3]

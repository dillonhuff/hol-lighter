module Logic(mkT, mkF, mkAnd, mkOr, mkImp, mkForall, mkExists, mkNeg,
             t, f, (/\), (\/), (-->), forall, exists, neg) where

import Term
import Type

mkT =
  let x = mkVar "x" o in
   mkEq (mkLam x x) (mkLam x x)

t = mkT

mkF =
  let x = mkVar "x" o in
  mkEq (mkLam x t) (mkLam x x)

f = mkF

mkAnd =
  let p = mkVar "p" o
      q = mkVar "q" o
      f = mkVar "f" (func o (func o o))
      fpq = mkLam f (mkApp (mkApp f p) q)
      ftt = mkLam f (mkApp (mkApp f mkT) mkT) in
   mkLam p (mkLam q (mkEq fpq ftt))

(/\) :: Term -> Term -> Term
(/\) a b = mkApp (mkApp mkAnd a) b

mkImp =
  let p = mkVar "p" o
      q = mkVar "q" o in
   mkLam p (mkLam q (mkEq (p /\ q) p))

(-->) a b = mkApp (mkApp mkImp a) b

mkForall t =
  let p = mkVar "P" (func t o) in
   mkLam p (mkEq p (mkLam (mkVar "x" t) mkT))

forall f =
  mkApp (mkForall $ leftType $ typeOf f) f

mkExists t =
  let p = mkVar "P" (func t o)
      q = mkVar "Q" o
      x = mkVar "x" t in
   mkLam p (forall (mkLam q $ (forall (mkLam x $ (mkApp p x) --> q)) --> q))

exists f =
  mkApp (mkExists $ leftType $ typeOf f) f

mkNeg =
  mkLam (mkVar "t" o) ((mkVar "t" o) --> mkF)

neg a = mkApp mkNeg a

mkOr =
  let p = mkVar "p" o
      q = mkVar "q" o
      r = mkVar "r" o in
   mkLam p $ mkLam q $ forall $ mkLam r $ (p --> r) --> (q --> r) --> r

(\/) a b = mkApp (mkApp mkOr a) b

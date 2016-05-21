module Logic(mkT, mkF, mkAnd, mkOr, mkImp, mkForall, mkExists, mkNeg,
             bool, mkEq,
             t, f, (/\), (\/), (-->), forall, exists, neg) where

import Term
import Type

bool = tp "bool"

mkEq a b =
  case typeOf a == typeOf b of
   True -> mkApp (mkApp (mkCon "=" (func (typeOf a) (func (typeOf a) bool))) a) b
   False -> error $ "mkEq: arguments have different types " ++ show a ++ " " ++ show b

mkT =
  let x = mkVar "x" bool in
   mkEq (mkLam x x) (mkLam x x)

t = mkT

mkF =
  let x = mkVar "x" bool in
  mkEq (mkLam x t) (mkLam x x)

f = mkF

mkAnd =
  let p = mkVar "p" bool
      q = mkVar "q" bool
      f = mkVar "f" (func bool (func bool bool))
      fpq = mkLam f (mkApp (mkApp f p) q)
      ftt = mkLam f (mkApp (mkApp f mkT) mkT) in
   mkLam p (mkLam q (mkEq fpq ftt))

(/\) :: Term -> Term -> Term
(/\) a b = mkApp (mkApp mkAnd a) b

mkImp =
  let p = mkVar "p" bool
      q = mkVar "q" bool in
   mkLam p (mkLam q (mkEq (p /\ q) p))

(-->) a b = mkApp (mkApp mkImp a) b

mkForall t =
  let p = mkVar "P" (func t bool) in
   mkLam p (mkEq p (mkLam (mkVar "x" t) mkT))

forall f =
  mkApp (mkForall $ leftType $ typeOf f) f

mkExists t =
  let p = mkVar "P" (func t bool)
      q = mkVar "Q" bool
      x = mkVar "x" t in
   mkLam p (forall (mkLam q $ (forall (mkLam x $ (mkApp p x) --> q)) --> q))

exists f =
  mkApp (mkExists $ leftType $ typeOf f) f

mkNeg =
  mkLam (mkVar "t" bool) ((mkVar "t" bool) --> mkF)

neg a = mkApp mkNeg a

mkOr =
  let p = mkVar "p" bool
      q = mkVar "q" bool
      r = mkVar "r" bool in
   mkLam p $ mkLam q $ forall $ mkLam r $ (p --> r) --> (q --> r) --> r

(\/) a b = mkApp (mkApp mkOr a) b

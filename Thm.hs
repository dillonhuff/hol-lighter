module Thm() where

import Data.List as L

import Logic
import Term
import Type

data Thm
  = Thm [Term] Term
    deriving (Eq, Ord)

thm hs t =
  case L.and $ L.map (\t -> typeOf t == o) $ t:hs of
   True -> Thm hs t
   False -> error $ "thm: bad arguments " ++ show hs ++ " " ++ show t

hyp (Thm hs _) = hs
con (Thm _ c) = c

refl t = thm [] (mkEq t t)

trans thm1 thm2 =
  let (s, t) = decEq $ con thm1
      (v, u) = decEq $ con thm2 in
   case t == v of
    True -> thm ((hyp thm1) ++ (hyp thm2)) (mkEq s u)
    False -> error $ "trans: bad arguments " ++ show thm1 ++ " " ++ show thm2

comb thm1 thm2 =
  let (s, t) = decEq $ con thm1
      (u, v) = decEq $ con thm2 in
   thm ((hyp thm1) ++ (hyp thm2)) (mkEq (mkApp s u) (mkApp t v))

abs thm1 x =
  case not $ L.or $ L.map (\h -> isFreeIn x h) $ hyp thm1 of
   True ->
     let (s, t) = decEq $ con thm1 in
      thm (hyp thm1) (mkEq (mkLam x s) (mkLam x t))
   False -> error $ "abs: bad arguments " ++ show thm1 ++ " " ++ show x

beta x t =
  thm [] (mkEq (mkApp (mkLam x t) x) t)

assume p = thm [p] p

mp thm1 thm2 =
  let (p, q) = decEq $ con thm1
      u = con thm2 in
   case u == p of
    True -> thm ((hyp thm1) ++ (hyp thm2)) q
    False -> error $ "mp: bad arguments " ++ show thm1 ++ " " ++ show thm2

antiSym thm1 thm2 =
  let p = con thm1
      q = con thm2
      g = hyp thm1
      d = hyp thm2 in
   thm ((L.filter (\h -> h /= q) g) ++ (L.filter (\h -> h /= p) d)) (mkEq p q)

inst thm1 x t =
  let p = con thm1
      hs = hyp thm1 in
   case isVar x of
    True -> thm (L.map (\h -> subVar x t h) hs) (subVar x t p)
    False ->
      error $ "inst: bad arguments " ++ show thm1 ++ " " ++ show x ++ " " ++ show t

ext t x =
  thm [] (mkEq (mkLam x (mkApp t x)) t)

inf =
  let f = mkVar "f" (func i i)
      x = mkVar "x" i
      x1 = mkVar "x1" i
      x2 = mkVar "x2" i
      y = mkVar "y" i
      fx1 = mkApp f x1
      fx2 = mkApp f x2
      isInjective =
        forall $ mkLam x1 $ forall $ mkLam x2 $ (mkEq fx1 fx2) --> (mkEq x1 x2)
      isSurjective =
        forall $ mkLam y $ exists $ mkLam x $ mkEq y (mkApp f x) in
  thm [] (exists $ mkLam f (isInjective /\ (neg $ isSurjective)))

choice p =
  let x = mkVar "x" (leftType $ typeOf p) in
   thm [] (forall $ mkLam x $ (mkApp p x) --> (mkApp p (c $ mkLam x $ mkApp p x)))

instance Show Thm where
  show (Thm hs c) = showCommaList hs ++ " |- " ++ show c

showCommaList :: (Show a) => [a] -> String
showCommaList ls = L.concat $ L.intersperse ", " $ L.map show ls

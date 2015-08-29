module Thm() where

import Data.List as L

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

instance Show Thm where
  show (Thm hs c) = showCommaList hs ++ " |- " ++ show c

showCommaList :: (Show a) => [a] -> String
showCommaList ls = L.concat $ L.intersperse ", " $ L.map show ls

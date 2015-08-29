module PrettyPrint(ppr) where

import Logic
import Term

ppr :: Term -> String
ppr t = firstToMatch pprOptions t

pprOptions =
  [(\t -> "T", \t -> t == mkT),
   (\t -> "F", \t -> t == mkF),
   (pprBinop "=", matchesBinop isEq),
   (pprBinop "\\/", matchesBinop (\t -> t == mkOr)),
   (pprBinop "/\\", matchesBinop (\t -> t == mkAnd)),
   (pprBinop "-->", matchesBinop (\t -> t == mkImp)),
   (pprUnop "~", matchesUnop (\t -> t == mkNeg)),
   (pprUnop "exists ", \t -> isApp t && (t == exists (snd $ decApp t))),
   (pprUnop "forall ", \t -> isApp t && (t == forall (snd $ decApp t))),
   (show, isVar),
   (show, isCon),
   (pprApp, isApp),
   (pprLam, isLam)]

pprApp t =
  let (a, b) = decApp t in
   "(" ++ ppr a ++ " " ++ ppr b ++ ")"

pprLam t =
  let (v, a) = decLam t in
   "(\\" ++ ppr v ++ ". " ++ ppr a ++ ")"

firstToMatch :: [(a -> b, a -> Bool)] -> a -> b
firstToMatch [] a = error "firstToMatch: out of options"
firstToMatch ((f, m):ms) a =
  case m a of
   True -> f a
   False -> firstToMatch ms a

matchesBinop :: (Term -> Bool) -> Term -> Bool
matchesBinop m t =
  case isApp t of
   True ->
     let (a, b) = decApp t in
      case isApp a of
       True ->
         let (r, x) = decApp a in
          m r
       False -> False
   False -> False

matchesUnop :: (Term -> Bool) -> Term -> Bool
matchesUnop m t =
  case isApp t of
   True ->
     let (a, b) = decApp t in
      m a
   False -> False

pprBinop :: String -> Term -> String
pprBinop bStr t =
  let (a, b) = decApp t
      (r, x) = decApp a in
   "(" ++ ppr x ++ " " ++ bStr ++ " " ++ ppr b ++ ")"

pprUnop :: String -> Term -> String
pprUnop uStr t =
  let (a, b) = decApp t in
   uStr ++ ppr b


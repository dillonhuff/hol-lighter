module Term(Term,
            mkVar, mkCon, mkApp, mkLam, mkEq,
            isVar, isCon, isLam, isApp,
            isEq, isConWithName,
            decApp, decLam, decEq,
            isFreeIn,
            subVar,
            typeOf) where

import Type

data Term
  = Var String Type
  | Con String Type
  | App Term Term
  | Lam Term Term
    deriving (Eq, Ord)

mkVar s t = Var s t
mkCon s t = Con s t
mkApp a b =
  case leftType (typeOf a) == typeOf b of
   True -> App a b
   False -> error $ "mkApp: bad arguments " ++ show a ++ " " ++ show b
mkLam v t =
  case v of
   Var _ _ -> Lam v t
   _ -> error $ "mkLam: bad arguments " ++ show v ++ " " ++ show t

mkEq a b =
  case typeOf a == typeOf b of
   True -> mkApp (mkApp (mkCon "=" (func (typeOf a) (func (typeOf a) o))) a) b
   False -> error $ "mkEq: arguments have different types " ++ show a ++ " " ++ show b

isEq t = isConWithName "=" t

isConWithName n (Con s _) = n == s
isConWithName _ _ = False

isVar (Var _ _) = True
isVar _ = False

isCon (Con _ _) = True
isCon _ = False

isApp (App _ _) = True
isApp _ = False

isLam (Lam _ _) = True
isLam _ = False

decEq (App (App (Con "=" t) a) b) = (a, b)

decApp (App a b) = (a, b)
decApp t = error $ "decApp: bad argument " ++ show t

decLam (Lam v t) = (v, t)

subVar targ res c@(Con _ _) = c
subVar targ res v@(Var _ _) =
  case v == targ of
   True -> res
   False -> v
subVar targ res (App a b) =
  App (subVar targ res a) (subVar targ res b)
subVar targ res (Lam v t) =
  case v == targ of
   True -> Lam v t
   False -> Lam v (subVar targ res t)

typeOf (Var _ t) = t
typeOf (Con _ t) = t
typeOf (App a b) = rightType $ typeOf a
typeOf (Lam v t) = func (typeOf v) (typeOf t)

isFreeIn x t = error "isFreeIn"

instance Show Term where
  show (Var n _) = n
  show (Con n _) = n
  show (App a b) = "(" ++ show a ++ " " ++ show b ++ ")"
  show (Lam v t) = "(\\" ++ show v ++ ". " ++ show t ++ ")"

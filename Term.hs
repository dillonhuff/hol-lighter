module Term(Term,
            mkVar, mkCon, mkApp, mkLam, mkEq,
            decEq,
            isFreeIn,
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

decEq (App (App (Con "=" t) a) b) = (a, b)

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

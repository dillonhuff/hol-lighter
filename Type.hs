module Type(Type,
            tp, func,
            isFuncType,
            leftType, rightType) where

data Type
  = T String
  | Func Type Type
    deriving (Eq, Ord)

tp s = T s
func l r = Func l r

isFuncType (Func _ _) = True
isFuncType _ = False

rightType (Func _ r) = r

leftType (Func l _) = l
leftType t = error $ "leftType: " ++ show t

instance Show Type where
  show (T s) = s
  show (Func l r) = "(" ++ show r ++ show l ++ ")"


data Term =
      MyTrue
    | MyFalse
    | Error -- なんとなく追加
    | IfThenElse Term Term Term
    | Zero
    | Succ Term
    | Pred Term
    | IsZero Term
    deriving(Show, Eq)

-- 数値型?
isNum :: Term -> Bool
isNum Zero     = True
isNum (Succ t) = isNum t
isNum _        = False

-- 値型?
isVal :: Term -> Bool
isVal MyTrue = True
isVal MyFalse = True
isVal t = (isNum t)
isVal _ = False

eval :: Term -> Term
eval MyTrue = MyTrue
eval MyFalse = MyFalse
eval (IfThenElse t1 t2 t3) = if (eval t1 == MyTrue) then (eval t2) else (eval t3)
eval Zero = Zero
eval (Succ t) = if (isNum t) then evalS (eval t) else Error
eval (Pred t) = if (isNum t) then evalP (eval t) else Error
eval (IsZero t) = if ((eval t) == Zero) then MyTrue else MyFalse

-- eval (Succ t)
--     | (eval t == Pred _) = t
--     | otherwise = Succ t
-- eval (Pred t)
--     | (eval t == Succ _) = t
--     | otherwise = Pred t


evalP :: Term -> Term
evalP (Succ t) = t
evalP t = Pred t

evalS :: Term -> Term
evalS (Pred t) = t
evalS t = Succ t
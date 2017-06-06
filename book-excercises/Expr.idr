module Expr

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
%name Expr expr, expr1, expr2

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add expr expr1) = (evaluate expr) + (evaluate expr1)
evaluate (Sub expr expr1) = (evaluate expr) - (evaluate expr1)
evaluate (Mult expr expr1) = (evaluate expr) * (evaluate expr1)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing orig@(Just x) = orig
maxMaybe orig@(Just x) Nothing = orig
maxMaybe origx@(Just x) origy@(Just y) = case compare x y of
                                            LT => origy
                                            EQ => origx
                                            GT => origx

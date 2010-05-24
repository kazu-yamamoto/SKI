module Exp where

data Exp = S | K | Var Char | Exp :. Exp deriving Eq

infixl 6 :.

instance Show Exp where
    show S = "S"
    show K = "K"
    show (Var c) = [c]
    show (x :. y) = "(" ++ show x ++ show y ++ ")"

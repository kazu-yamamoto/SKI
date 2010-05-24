module Eval (
    evaluate
  ) where

import Exp
import Stack

----------------------------------------------------------------

evaluate :: Exp -> Exp
evaluate exp0 = eval exp0 empty

eval :: Exp -> Stack Exp -> Exp
eval (exp1 :. exp2) stack = eval exp1 $ push exp2 stack
eval exp1 stack
  | isEmpty stack = exp1
eval exp1@(Var _) stack = foldl (:.) exp1 $ map evaluate stack
eval S stack = eval (x :. z :. (y :. z)) stack3
  where
    (x,stack1) = pop stack
    (y,stack2) = pop stack1
    (z,stack3) = pop stack2
eval K stack = eval x stack2
  where
    (x,stack1) = pop stack
    (_,stack2) = pop stack1

----------------------------------------------------------------


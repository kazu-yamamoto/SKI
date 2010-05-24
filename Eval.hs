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
eval exp1           stack = exec stack exp1

exec :: Stack Exp -> Exp -> Exp
exec stack0 exp1
  | isEmpty stack0 = exp1
exec stack0 exp1@(Var _) = foldl (:.) exp1 $ map evaluate stack0
exec stack0 S = eval (x :. z :. (y :. z)) stack3
  where
    (x,stack1) = pop stack0
    (y,stack2) = pop stack1
    (z,stack3) = pop stack2
exec stack0 K = eval x stack2
  where
    (x,stack1) = pop stack0
    (_,stack2) = pop stack1
exec stack0 exp0@(_ :. _) = eval exp0 stack0

----------------------------------------------------------------


module Stack (
    Stack
  , pop, push, empty, isEmpty
  ) where

type Stack a = [a]

pop :: Stack a -> (a, Stack a)
pop (x:xs) = (x,xs)
pop _ = error "pop"

push :: a -> Stack a -> Stack a
push x xs = x : xs

empty :: Stack a
empty = []

isEmpty :: Eq a => Stack a -> Bool
isEmpty = (==empty)

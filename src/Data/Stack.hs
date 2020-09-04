module Data.Stack (
  Stack
  , empty
  , singleton
  , fromList 
  , push
  , pop
) where

-- datatype  
data Stack a = Stack [a] | EmptyStack deriving (Show, Eq)

-- | get an empty stack
empty :: Stack a
empty = EmptyStack

-- | create a stack with one element
singleton :: a -> Stack a
singleton elem = Stack [elem]

-- | create stack from list; the first list element is popped first
fromList :: [a] -> Stack a
fromList [] = EmptyStack
fromList xs = Stack xs

-- | pop the last pushed element from stack
pop :: Stack a -> (Maybe a, Stack a)
pop (Stack (x:xs)) = (Just x, fromList xs)
pop EmptyStack = (Nothing, empty)

-- | push one element onto the stack
push :: Stack a -> a -> Stack a
push (Stack xs) x = fromList (x:xs)
push EmptyStack x = singleton x

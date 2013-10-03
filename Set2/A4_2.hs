module A4_2 where

-- a nested datatype for square matrices
type Square a    = Square' Nil a
data Square' t a = Zero (t (t a)) | Succ (Square' (Cons t) a)
data Nil a       = Nil deriving Show
data Cons t a    = Cons a (t a) deriving Show


-- examples
m :: Square Int
m = Zero Nil

-- No idea what this represents, but at least it type checks -_-
n :: Square Int
n = Succ (Zero (Cons (Cons 1 Nil) Nil))

--l :: Square Int
l = Zero (Cons (Cons 1 Nil) Nil)

--v :: Cons t a
c = Cons 1 (Cons 2 Nil)

-- Matty's thinking:
{-
- Square' t a is the real matrix thingy, Square is just a nice wrapper to provide the base case argument.
- The Square' has two options, one is something, the other one is again a square matrix.
- Thus, the 'something' must probably be the first row and column of square matrix
- As we see, the Zero thingy, has two t's, yay, that makes sense after my previous comment o_O
- Apparently we can use Cons and Nil to add a value to the column/row vector
-}


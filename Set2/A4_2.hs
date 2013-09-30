module A4_2 where

-- a nested datatype for square matrices
type Square a    = Square' Nil a
data Square' t a = Zero (t (t a)) | Succ (Square' (Cons t) a)
data Nil a       = Nil
data Cons t a    = Cons a (t a)


-- examples
m :: Square Int
m = Zero Nil

-- No idea what this represents, but at least it type checks -_-
n :: Square Int
n = Succ (Zero (Cons ((Cons 1) Nil) Nil))

--l :: Square Int
--l = Succ (Succ (Zero (Cons ((Cons 1) Nil) Nil)))

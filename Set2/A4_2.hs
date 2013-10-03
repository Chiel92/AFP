module A4_2 where

-- a nested datatype for square matrices
type Square a    = Square' Nil a
data Square' t a = Zero (t (t a)) | Succ (Square' (Cons t) a)
data Nil a       = Nil
data Cons t a    = Cons a (t a)

-- The 1D identity matrix
i1 :: Square Int
i1 = Succ (Zero (Cons e1 Nil)) where
    e1 = Cons 1 Nil

-- The 2D identity matrix
i2 :: Square Int
i2 = Succ (Succ (Zero (Cons e1 (Cons e2 Nil)))) where
    e1 = Cons 1 (Cons 0 Nil)
    e2 = Cons 0 (Cons 1 Nil)

-- The 123456789 matrix :)
m3 :: Square Int
m3 = Succ (Succ (Succ (Zero (Cons v1 (Cons v2 (Cons v3 Nil)))))) where
    v1 = Cons 1 (Cons 4 (Cons 7 Nil))
    v2 = Cons 2 (Cons 5 (Cons 8 Nil))
    v3 = Cons 3 (Cons 6 (Cons 9 Nil))


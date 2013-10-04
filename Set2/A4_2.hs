module A4_2 where

-- A nested datatype for square matrices
-- We use row vectors, because that reads conveniently in the code
type Square a    = Square' Nil a
data Square' t a = Zero (t (t a)) | Succ (Square' (Cons t) a)
data Nil a       = Nil deriving Show
data Cons t a    = Cons a (t a) deriving Show


-- The 2D identity matrix
i2 :: Square Int
i2 = Succ (Succ (Zero (Cons e1 (Cons e2 Nil)))) where
    e1 = Cons 1 (Cons 0 Nil)
    e2 = Cons 0 (Cons 1 Nil)

-- The 123456789 matrix :)
m3 :: Square Int
m3 = Succ (Succ (Succ (Zero (Cons r1 (Cons r2 (Cons r3 Nil)))))) where
    r1 = Cons 1 (Cons 2 (Cons 3 Nil))
    r2 = Cons 4 (Cons 5 (Cons 6 Nil))
    r3 = Cons 7 (Cons 8 (Cons 9 Nil))


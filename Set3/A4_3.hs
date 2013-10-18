{-# LANGUAGE FlexibleInstances, RankNTypes, TypeSynonymInstances, ImpredicativeTypes #-}
module A4_3 where

-- A nested datatype for square matrices
-- We use row vectors, because that reads conveniently in the code
type Boool       = Bool
type Square      = Square' Nil
data Square' t a = Zero (t (t a)) | Succ (Square' (Cons t) a)
data Nil a       = Nil deriving Show
data Cons t a    = Cons a (t a) deriving Show


-- The 1D identity matrix (also known as the number one)
i1 :: Square Int
i1 = Succ (Zero (Cons e0 Nil)) where
    e0 = Cons 1 Nil

-- The 2D identity matrix
i2 :: Square Int
i2 = Succ (Succ (Zero (Cons e1 (Cons e2 Nil)))) where
    e1 :: Cons (Cons Nil) Int
    e1 = Cons 1 (Cons 0 Nil)
    e2 = Cons 0 (Cons 1 Nil)

-- The 123456789 matrix :)
m3 :: Square Int
m3 = Succ (Succ (Succ (Zero (Cons r1 (Cons r2 (Cons r3 Nil)))))) where
    r1 = Cons 1 (Cons 2 (Cons 3 Nil))
    r2 = Cons 4 (Cons 5 (Cons 6 Nil))
    r3 = Cons 7 (Cons 8 (Cons 9 Nil))

m3' :: Square Int
m3' = Succ (Succ (Succ (Zero (Cons r1 (Cons r2 (Cons r3 Nil)))))) where
    r1 = Cons 0 (Cons 1 (Cons 2 Nil))
    r2 = Cons 3 (Cons 4 (Cons 5 Nil))
    r3 = Cons 6 (Cons 7 (Cons 8 Nil))

test :: Bool
test = m3 == fmap (+1) m3'


-- Equality stuff
eqNil :: (a -> a -> Bool) -> (Nil a -> Nil a -> Bool)
eqNil eqA Nil Nil = True

-- eqCons :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool)) ->
--           (a -> a -> Bool) ->
--           (Cons t a -> Cons t a -> Bool)
eqCons :: ( (a -> a -> Boool) -> (t a -> t a -> Boool) ) ->
          (a -> a -> Boool) ->
          (Cons t a -> Cons t a -> Boool)
eqCons eqT eqA (Cons x xs) (Cons y ys) = eqA x y && eqT eqA xs ys


-- Task. A type with a forall on the inside requires the extension RankNTypes to be enabled.  Try to understand what the difference is between a function of the type of eqCons and a function with the same type but the forall omitted. Can you omit the forall in the case of eqCons and does the function still work?
-- Answer: Yes, you can omit the forall and the function still works, if you change the forall b into an a. It works because the first parameter of eqCons always is eqNil (as provided in the eqSquare defenition).


eqSquare' :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool)) ->
                (a -> a -> Bool) ->
                (Square' t a -> Square' t a -> Bool)
eqSquare' eqT eqA (Zero xs) (Zero ys) = eqT (eqT eqA) xs ys
eqSquare' eqT eqA (Succ xs) (Succ ys) = eqSquare' (eqCons eqT) eqA xs ys
eqSquare' eqT eqA _         _         = False

-- Task. Again, try removing the forall from the type of eqSquare'. Does the function still typecheck? Try to explain!
-- Answer: No, you can't omit the forall, exactly because the function doesn't typecheck anymore. It doesn't typecheck anymore because the function (and it's type) changes.

eqSquare :: (a -> a -> Bool) -> Square a -> Square a -> Bool
eqSquare = eqSquare' eqNil


instance Eq a => Eq (Square a) where
    (==) = eqSquare (==)


-- Task. Systematically follow the scheme just presented in order to define a Functor instance for square matrices. I.e., derive a function mapSquare such that you can define
mapNil :: (a -> b) -> Nil a -> Nil b
mapNil _ Nil = Nil

mapCons :: ((a -> b) -> (t a -> t b)) -> (a -> b) -> Cons t a -> Cons t b
mapCons mapT f (Cons x xs) = Cons (f x) (mapT f xs)

mapSquare' :: (forall a b. (a -> b) -> (t a -> t b) ) ->
              (a -> b) ->
              Square' t a -> Square' t b
mapSquare' mapT f (Zero xs) = Zero (mapT (mapT f) xs)
mapSquare' mapT f (Succ xs) = Succ (mapSquare' (mapCons mapT) f xs)

mapSquare :: (a -> b) -> Square a -> Square b
mapSquare = mapSquare' mapNil

instance Functor Square where
    fmap = mapSquare


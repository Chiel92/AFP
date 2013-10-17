{-# LANGUAGE GADTs, KindSignatures #-}
module A6_1 where

data Contract :: * -> * where
    Pred :: (a -> Bool) -> Contract a
    Fun  :: Contract a -> Contract b -> Contract (a -> b)
    DFun :: Contract a -> (a -> Contract b) -> Contract (a -> b)

assert :: Contract a -> a -> a
assert (Pred p) x        = if p x then x else error "contract violation"
assert (Fun pre post) f  = assert post . f . assert pre
assert (DFun pre post) f = (\x -> (assert (post x) . f . assert pre) x)


-- Task: Define a contract for which the equation assert true x == x holds
true :: Contract a
true = Pred (\_ -> True)

-- The proof:
-- assert true x
-- assert (Pred (\_ -> True)) x
-- if (_ -> True) x then x else error "contract violation"
-- if True then x else error "contract violation"
-- x


-- Task: Define a combinator that reexpresses the behaviour of the old Fun constructor in terms of the new and more general one
-- (-->) :: Contract a -> Contract b -> Contract (a -> b)
-- (-->) (Fun pre post) f = DFun pre (\_ -> post)


-- Task: Define a contract suitable for the list index function
validIndex :: Contract ([a] -> Int -> a)
validIndex = undefined


-- Test cases
pos :: (Num a, Ord a) => Contract a
pos = Pred (>=0)

neg = Pred (<= 0)

dtrue :: a -> Contract a
dtrue _ = Pred (\_ -> True)

myFun  = Fun pos true
myDFun = DFun pos dtrue

iszero = Fun pos neg


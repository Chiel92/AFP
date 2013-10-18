{-# LANGUAGE GADTs, KindSignatures #-}
module A6_1 where

data Contract :: * -> * where
    Pred :: (a -> Bool) -> Contract a
    Fun  :: Contract a -> Contract b -> Contract (a -> b)
    DFun :: Contract a -> (a -> Contract b) -> Contract (a -> b)
    List :: Contract a -> Contract [a]


assert :: Contract a -> a -> a
assert (Pred p) x        = if p x then x else error "contract violation"
assert (Fun pre post) f  = assert post . f . assert pre
assert (DFun pre post) f = (\x -> (assert (post x) . f . assert pre) x)
assert (List c) xs       = map (assert c) xs


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
(-->) :: Contract a -> Contract b -> Contract (a -> b)
(-->) c1 c2 = DFun c1 (\_ -> c2)


-- Task: Define a contract suitable for the list index function
validIndex :: Contract ([a] -> Int -> a)
validIndex = DFun true f where
    f xs = DFun true (\i -> Pred (\_ -> 0 <= i && i < length xs))

-- Some test cases for validIndex
testValidIndex  = assert validIndex (!!) [1..7] 3
testValidIndex' = assert validIndex (!!) [1..7] 42


-- Task: Define a contract preseres where assert (preserves p) f x fails if and only if the value of p x is different from the value of p (f x).
preserves :: Eq b => (a -> b) -> Contract (a -> a)
preserves f = DFun true g where
    g x = Pred (\y -> f x == f y)


-- Task: Consider preservePos and preservePos' (as defined below)
pos :: (Num a, Ord a) => Contract a
pos = Pred (>0)

preservesPos :: Contract (Int -> Int)
preservesPos  = preserves (>0)
preservesPos' = pos --> pos

-- These two things are NOT the same try as counterexample:
counterexample  = assert preservesPos (+42) (-1337)  -- This one checks whether the sign of the value doesn't change
counterexample' = assert preservesPos' (+42) (-1337) -- This one checks whether all values are positive or not
-- For the fun of it, we have a proof with equational reasoning in A6_1_proof.txt


-- Task: consider things
allPos = List pos
allPos' = Pred (all (>0))

-- The difference is that allPos lazily computes the elements, while allPos' computes them all before returning the result.
-- 
-- In general the only differences is in strictness and non-strictness.
-- In the infinite list case this means you actually get a different result.
-- For example take 5 $ allPos [1..] works, where take 5 $ allPos' [1..] doesn't.



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
-- validIndex :: Contract ([a] -> Int -> a)
-- validIndex = Pred f where
--     f xs n _ = n >= 0 && n < length xs 

-- GOAL: assert validIndex [1,2,3] 42 does not compile


-- Task: Define a contract preseres where assert (preserves p) f x fails if and only if the value of p x is different from the value of p (f x).
preserves :: Eq b => (a -> b) -> Contract (a -> a)
preserves f = DFun true g where
    g x = Pred (\y -> f x == f y)


-- Task: Consider preservePos and preservePos' (as defined below)
preservesPos  = preserves (>0)
preservesPos' = pos --> pos

--
-- These two things are NOT the same try as counterexample:
counterexample  = assert preservesPos (+42) (-1337)
counterexample' = assert preservesPos' (+42) (-1337)

-- But for the fun of it, here a (part of) the proof that these two things don't give the same result when used in assert:
-- assert presevesPos
-- = { definition preservesPos }
-- assert (DFun true (\x -> Pred (\y -> (>0) x == (>0) y)))
-- = {TODO}
-- assert (DFun (Pred (\_ -> True)) (\x -> Pred (\y -> (>0) x == (>0) y)))
-- = { definition assert (DFun pre post) }
-- (\f x -> (assert ((\x' -> Pred (\y -> (>0) x' == (>0) y)) x) . f . assert (Pred (\_ -> True))) x)
-- = { lambda weggewerkt TODO }
-- (\f x -> (assert (Pred (\y -> (>0) x == (>0) y)) . f . assert (Pred (\_ -> True))) x)
-- = { definition assert (Pred p) }
-- (\f x -> (assert (Pred (\y -> (>0) x == (>0) y)) . f . (                                                  )) x)
--                                                        (\x' -> if (\_ -> True) x' then x' else error "c v")
-- = { LAMBDA INGEVULD TODO }
--                                                        (\x' -> if True then x' else error "c v")
-- = { definition if }
--                                                        (\x' -> x')
-- = { function composition identity }
-- (\f x -> (assert (Pred (\y -> (>0) x == (>0) y)) . f) x)
-- = { definition assert (Pred p) }
-- (\f x -> ((                                                              ) . f) x)
--           (\x' -> if (\y -> (>0) x == (>0) y) x' then x' else error "c v")
-- = { lambda uitgewerkt TODO }
--           (\x' -> if (>0) x == (>0) x' then x' else error "c v")
-- = { copy paste TODO }
-- (\f x -> ((\x' -> if (>0) x == (>0) x' then x' else error "c v") . f) x)
-- /= { *here the magic occurs* }
-- (\f x -> ((\x' -> if (>0) x' then x' else error "c v") . f . (\x' -> if (>0) x' then x' else error "c v")) x)
-- = { ? }
-- (\f x -> (assert (Pred (>0)) . f . assert (Pred (>0))) x)
-- = { LAMBDA UITBREIDEN TODO }
-- (\f x -> (assert ((\_ -> Pred (>0)) x) . f . assert (Pred (>0))) x)
-- assert (DFun (Pred (>0)) (\_ -> Pred (>0)))
-- = {TODO}
-- assert (DFun pos (\_ -> pos))
-- = { definition (-->) }
-- assert (pos --> pos)
-- = { definition preservePos' }
-- assert preservesPos'


-- Task: consider things
allPos = List pos
allPos' = Pred (all (>0))

-- The infinite list case: allPos lazily computes the elements, while allPos' computes them all before returning the results. So for example take 5 $ allPos [1..] works, where take 5 $ allPos' [1..] doesn't.



-- Test cases
pos :: (Num a, Ord a) => Contract a
pos = Pred (>0)

dtrue :: a -> Contract a
dtrue _ = true

myFun  = Fun pos true
myDFun = DFun pos dtrue


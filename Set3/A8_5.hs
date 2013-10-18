module A8_3 where

-- This example is found on the internet:
-- http://www.mail-archive.com/haskell-cafe@haskell.org/msg65869.html

-- It works because the duplicate funtion duplicates the type signature (the amount of a's in the type signature doubles every time you 'add another dup function').
-- The amount of a's in the type signature is 2^n, where n is the number of composed dub's, which is exponential..
-- Because this doesn't only duplicate the number of a's, but also wraps it in different tuples every time (with two different types - one normal and one singleton list), this is a way to create a truly large type signature :)

dup :: a -> (a, [a])
dup x = (x, [x])

test1  = dup
test2  = dup.dup
test4  = dup.dup.dup.dup
test8  = dup.dup.dup.dup.dup.dup.dup.dup
test12 = dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup
test16 = dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup
--test20 = dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup
--test24 = dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup
--test50 = dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup


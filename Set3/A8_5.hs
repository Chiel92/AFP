module A8_3 where

-- This example is copied from the internet:
-- https://www.google.nl/search?q=haskell+exponentiol+type
-- http://www.mail-archive.com/haskell-cafe@haskell.org/msg65869.html

dup x = (x,x)

test1  = dup
test2  = dup.dup
test3  = dup.dup.dup
test6  = dup.dup.dup.dup.dup.dup
test9  = dup.dup.dup.dup.dup.dup.dup.dup.dup
test12 = dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup
-- test24 = dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.
-- test48 = dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dupdup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dup.dupdup.dup.dup.dup

-- TODO: an explanation why it's exponential (like:
--       This is exponential because the amount of a's is 2^n, and thus the time it takes to type check this is 2^n, which is exponential
-- )
-- Another TODO is to try this idea with different types - (the truly large type :))

-- P.S. test24 is horrible on ghci, but maybe it's not horrible enough when you actually compile it - for the ones who volunteer to try that, I've made a test48, just to be sure :)


module A2_5 where

--test :: [Int]
--test = [count 1, count 1 2 3, count "" [True, False] id (+)]

--count = \x -> case x of
--     () -> 0
--     otherwise -> count

class CountArgs r where
    count :: a -> r

instance CountArgs Int where
    count = \x -> 0

instance (CountArgs r) => CountArgs (a -> r) where
    count x = count

test' :: [Int]
test' = [count 1, count 1 2 3, count "" [True, False] id (+)]

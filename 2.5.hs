module A2_5 where

test :: [Int]
test = [count, count 1 2 3, count "" [True, False] id (+)]

count = \x -> case x of
     () -> 0
     otherwise -> count

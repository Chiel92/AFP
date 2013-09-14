module A2_5 where

-- First part: simply ignore the arguments and always return 0
class IgnoreArgs r where
    count :: a -> r

instance IgnoreArgs Int where
    count x = 0

instance (IgnoreArgs r) => IgnoreArgs (a -> r) where
    count x = count

test :: [Int]
{-test = [count, count 1 2 3, count "" [True, False] id (+)]-}
test = [count 1, count 1 2 3, count "" [True, False] id (+)]

-- Second part: count the number arguments that have been passed
class CountArgs r where
    count' :: a -> r
    foo :: Int -> a -> r

instance CountArgs Int where
    count' x = 1
    foo n x = (n+1)


instance (CountArgs r) => CountArgs (a -> r) where
    count' x = foo 0 x
    foo n x = foo (n+1)

test' :: [Int]
{-test' = [count', count' 1 2 3, count' "" [True, False] id (+)]-}
test' = [count' 1, count' 1 2 3, count' "" [True, False] id (+)]

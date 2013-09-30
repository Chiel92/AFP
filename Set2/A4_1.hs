module A4_2 where

-- data F a = F {unF :: F a -> a}
data F a = F (F a -> a)

unF :: F a -> F a -> a
unF (F f) = f

-- y f = (\x -> f x x) (\x -> f x x)
y' (F f) = 

-- y = \f -> (\x -> f (x x)) (\x -> f (x x))
-- y f = let g x = f x x in g g


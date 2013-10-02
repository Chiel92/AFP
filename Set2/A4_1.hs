module A4_1 where

-- data F a = F {unF :: F a -> a}
data F a = F (F a -> a)

unF :: F a -> F a -> a
unF (F f) = f

y :: (a -> a) -> a
y f = (\(F x) -> f (x (F x))) (F (\(F x) -> f (x (F x))))

-- y = \f -> (\x -> f (x x)) (\x -> f (x x))
-- y f = let g x = f (x x) in g g
-- y f = g g where g x = f (x x)


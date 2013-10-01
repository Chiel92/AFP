module A4_2 where

-- data F a = F {unF :: F a -> a}
data F a = F (F a -> a)

unF :: F a -> F a -> a
unF (F f) = f

-- y f = (\x -> f (x x)) (\x -> f (x x))

-- y is a function, that gets one parameter: f          - WRONG - it gets more than 1 parameter, it gets x as well, or something :S
-- f is a function, that gets one parameter: (x x)      - Except that that thing is a function again, so it should get more parameters...
-- x is a function, that gets one parameter: x (itself) - Which is, of course, a function...

-- y = \f -> (\x -> f (x x)) (\x -> f (x x))
-- y f = let g x = f (x x) in g g

y f = g g where g x = f (x x)


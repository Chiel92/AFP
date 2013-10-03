module A4_1 where

data F a = F {unF :: F a -> a}

y :: (a -> a) -> a
y f = (\(F x) -> f (x (F x))) (F (\(F x) -> f (x (F x))))


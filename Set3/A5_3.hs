{-# LANGUAGE FlexibleContexts #-}
module A5_3 where
import Control.Monad.Reader
import System.Random

one :: Int
one = 1

two :: Int
two = 2

{-
randomN :: (RandomGen g) => Int -> g -> Int
randomN n g = (fst (next g) `mod` (two * n + one)) - n

-- This is the most general type
sizedInt :: (MonadTrans t, MonadReader Int (t m), MonadReader g m, RandomGen g) =>
            t m Int
sizedInt = do
    n <- ask -- ask :: t m Int
    g <- lift ask -- ask :: m g
    return (randomN n g)
-}


-- Evidence translation for all the overloading involved in
-- the functions randomN and sizedInt. First, define the
-- record types for the classes involved.

data RandomGen g = RandomGen { next :: g -> (Int, g) }

data MonadReader a m = MonadReader { ask :: m a }

data MonadTrans t m a = MonadTrans { lift :: m a -> t m a }

-- Then translate randomN and sizedInt similar to the translation
-- on Slide 10-21. You are allowed to introduce local abbreviations
-- using let and where for often-used expressions.

-- TODO

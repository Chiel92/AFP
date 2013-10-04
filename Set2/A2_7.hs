{-# LANGUAGE FlexibleContexts #-}
module A2_7 where
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity


type Object a = a -> a -> a
data X = X {n :: Int, f :: Int -> Int}
data Step a b = Enter a
              | Return b
                deriving Show


fixObject :: Object a -> a
fixObject o = o (error "super") (fixObject o)

extendedBy :: Object a -> Object a -> Object a
extendedBy o1 o2 super this = o2 (o1 super this) this

fac :: Monad m => Object (Int -> m Int)
fac super this n = case n of
    0 -> return 1
    n -> liftM (n*) (this (n - 1))

calls :: MonadState Int m => Object (a -> m b)
calls super this n = do
    modify (+1)
    super n


-- The zero object
zero :: Object a
zero super this = this

-- The trace object
-- The exercise tells us to use the most general type, so I should get rid of the Int's later
trace :: MonadWriter [Step Int Int] m => Object (Int -> m Int)
trace super this n = do
    tell [Enter n]
    result <- super n
    tell [Return result]
    return result


-- The example from the pdf
test :: (Int, [Step Int Int])
test = runWriter (fixObject (fac `extendedBy` trace) 3)


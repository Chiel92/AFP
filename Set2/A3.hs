{-# LANGUAGE MultiParamTypeClasses #-}
module A3 where
import Control.Monad.State

data StateMonadPlus s a = StateMonadPlus (s -> Either String (a, s))

instance Monad (StateMonadPlus s) where
    m >>= k = StateMonadPlus f
      where
        f :: (s -> Either String (a, s))
        f s = g (runStateMonadPlus m s)
          where
            g :: Either String (a, s) -> Either String (a, s)
            g (Left s) = Left s
            g m'@(Right (a, s')) = runStateMonadPlus (k (\s -> m')) s'


    -- let Right (a, s') = runStateMonadPlus m s
    -- in k a
    -- m >>= k = StateMonadPlus $ \s ->
    -- let Right (a, s') = runStateMonadPlus m s
    -- in runStateMonadPlus (k a) s'
    return a = StateMonadPlus $ \s -> Right (a, s)


-- This function should count the number of binds (>>=)
-- and returns (and other primitive functions) that have been encountered,
-- including the call to diagnostics at hand.
diagnostics :: StateMonadPlus s String
diagnostics = undefined

-- Secondly, provide a function that
-- allows a user to annotate a computation with a given label.
-- The functions for
-- Features 2 and 3, as well as get and put,
-- should also be part of the diagnosis.
annotate :: String -> StateMonadPlus s a -> StateMonadPlus s a
annotate = undefined

-- Running the monad.
-- Given a computation in the StateMonadPlus and an initial
-- state, runStateMonadPlus returns either an error message
-- if the computation failed, or
-- the result of the computation and the final state.
runStateMonadPlus :: StateMonadPlus s a -> (s -> Either String (a, s))
runStateMonadPlus (StateMonadPlus f) = f


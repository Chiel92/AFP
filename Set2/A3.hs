{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module A3 where
import Control.Monad.State

data StateMonadPlus s a = StateMonadPlus (s -> Either String (a, s))

instance Monad (StateMonadPlus s) where
    -- (>>=) :: StateMonadPlus s a -> (a -> StateMonadPlus s a) -> StateMonadPlus s a
    m >>= k = StateMonadPlus (\s -> f (runStateMonadPlus m s))
      where
        f (Left s') = Left s'
        f (Right (a, s')) = runStateMonadPlus (k a) s'
    return a = StateMonadPlus (\s -> Right (a, s))

instance MonadState s (StateMonadPlus s) where
    -- get :: StateMonadPlus s s
    get = StateMonadPlus (\s -> Right (s, s))
    -- put :: s -> StateMonadPlus s ()
    put s = StateMonadPlus (\_ -> Right ((), s))


-- This function should count the number of binds (>>=)
-- and returns (and other primitive functions) that have been encountered,
-- including the call to diagnostics at hand.
diagnostics :: StateMonadPlus s String
diagnostics = undefined

-- Secondly, provide a function annotate that
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


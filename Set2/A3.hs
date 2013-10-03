{-# LANGUAGE MultiParamTypeClasses #-}
module A3 where
import Control.Monad.State


data StateMonadPlus s a = DOei

-- This function should count the number of binds (>>=)
--
-- and returns (and other primi-
-- tive functions) that have been encountered,
-- including the call to diagnostics at hand.
-- Secondly, provide a function
diagnostics :: StateMonadPlus s String
diagnostics = undefined

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
runStateMonadPlus :: StateMonadPlus s a -> s -> Either String (a, s)
runStateMonadPlus = undefined


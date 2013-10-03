{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies,NoMonomorphismRestriction #-}
module A3 where
import Control.Monad.State
import Data.List

type Dict = [(String, Int)]
type Stack s = [s]
data StateMonadPlus s a = StateMonadPlus ((s, Dict, Stack s) -> Either String (a, s, Dict, Stack s))


instance Monad (StateMonadPlus s) where
    -- (>>=) :: StateMonadPlus s a -> (a -> StateMonadPlus s a) -> StateMonadPlus s a
    m >>= k = StateMonadPlus (\(s, d, stack) -> f (runStateMonadPlus m (s, incDict "bind" d, stack)))
      where
        f (Left s') = Left s'
        f (Right (a, s', d, stack)) = runStateMonadPlus (k a) (s', d, stack)

    -- return :: a -> StateMonadPlus s a
    return a = StateMonadPlus (\(s, d, stack) -> Right (a, s, (incDict "return" d), stack))

    -- fail :: String -> StateMonadPlus s a
    fail message = StateMonadPlus (\(s, d, stack) -> Left message)


instance MonadState s (StateMonadPlus s) where
    -- get :: StateMonadPlus s s
    get = StateMonadPlus (\(s, d, stack) -> Right (s, s, d, stack))

    -- put :: s -> StateMonadPlus s ()
    put s = StateMonadPlus (\(_, d, stack) -> Right ((), s, d, stack))


instance Show (StateMonadPlus s String) where
    -- show :: StateMonadPlus s String -> String
    show (StateMonadPlus f) = g (f (undefined, [], [])) where
        g (Right (a, _, _, stack)) = show a
        g (Left s) = s

-- Allow saving the current state, and restoring a previous state as the current state
class MonadState s m => StoreState s m | m -> s where
    saveState :: m ()
    loadState :: m ()

instance StoreState s (StateMonadPlus s) where
    -- saveState :: StateMonadPlus s ()
    saveState = StateMonadPlus (\(s, d, stack) -> Right ((), s, d, s:stack))

    -- loadState :: StateMonadPlus s ()
    loadState = StateMonadPlus f where
        f (_, _, []) = fail "State stack is empty"
        f (_, d, s:stack) = Right ((), s, d, stack)


-- This function should count the number of binds (>>=)
-- and returns (and other primitive functions) that have been encountered,
-- including the call to diagnostics at hand.
-- The functions for Features 2 and 3, as well as get and put,
-- should also be part of the diagnosis.
diagnostics :: StateMonadPlus s String
diagnostics = StateMonadPlus (\(s, d, stack) ->
              let d'       = incDict "diagnostics" d
                  f (k, v) = k ++ "=" ++ (show v)
                  showd    = "[" ++ (intercalate ", " (map f d')) ++ "]"
              in Right (showd, s, d', stack))

-- Increment dictionary value for given key
incDict :: String -> Dict -> Dict
incDict key []                      = [(key, 1)]
incDict key ((k, v):xs) | k == key  = (k, v + 1):xs
                        | otherwise = (k, v):incDict key xs

-- Allow a user to annotate a computation with a given label.
annotate :: String -> StateMonadPlus s a -> StateMonadPlus s a
annotate key m = StateMonadPlus (\(s, d , stack) ->
                 let  Right (a, s', d',  stack) = runStateMonadPlus m (s, d, stack)
                      d''                = incDict key d'
                 in Right (a, s', d'',  stack))

-- Given a computation in the StateMona dPlus and an initial
-- state, runStateMonadPlus returns eit her an error message
-- if the computation failed, or
-- the result of the computation and the final state.
runStateMonadPlus :: StateMonadPlus s a -> (s, Dict, Stack s) -> Either String (a, s, Dict, Stack s)
runStateMonadPlus (StateMonadPlus f) = f


-- Testing
test = do
    return 3 >> return 4
    return 5
    diagnostics

test2 = do
    annotate "A" (return 3 >> return 4)
    return 5
    diagnostics

test3 = do
    annotate "A" (return 3 >> return 4)
    fail "We failed, but didn't get an exception"
    annotate "A" (return 3 >> return 4)
    return 5
    diagnostics

-- test4 :: (Num t, StoreState t m) => m (t, t, t, t, t)
test4 = do
    i1 <- get; saveState
    modify (*2)
    i2 <- get; saveState
    modify (*2)
    i3 <- get; loadState
    i4 <- get; loadState
    i5 <- get
    return (i1, i2, i3, i4, i5)


{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

module A11_2 where

import Generics.Deriving.Base
import Generics.Deriving.Instances ()


class GRead a where
    gread :: String -> a

instance GRead (U1 p) where
    gread _ = U1

instance (GRead (f p), GRead (g p)) => GRead ((:+:) f g p) where
    gread ('L':'1':xs) = (L1 $ gread xs)
    gread ('R':'1':xs) = (R1 $ gread xs)

instance (GRead (f p), GRead (g p)) => GRead ((:*:) f g p) where
    gread = let smurf = undefined in smurf


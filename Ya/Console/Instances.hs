{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
module Ya.Console.Instances where

import Ya

import "ghc-prim" GHC.Prim (State#, RealWorld)
import "ghc-prim" GHC.Types (IO (IO))

instance Mapping Straight Straight Arrow Arrow IO IO
 where mapping = rewrap / \m x -> bindIO x (returnIO `fio` m)

instance Mapping Straight Straight Arrow Arrow (IO `T'TT'I` L () IO) IO
 where mapping = rewrap / \m (T'TT'I ioio) -> bindIO (bindIO ioio unwrap) (returnIO `fio` m)

instance Mapping Straight Straight Arrow Arrow (Day Straight Arrow LM LM IO IO i ii) IO
 where mapping = rewrap / \from (U_V_UU_UUU_UUUU_T'TT'I_II_III (These (These x y) (U_I_II f))) -> bindIO x (\xx -> from `compose` f `compose` These xx `fo` y)

instance Mapping Straight Straight Arrow Arrow (Straight Arrow ()) IO
 where mapping = rewrap / \from (U_I_II f) -> returnIO `ha` from `li` f ()

returnIO :: a -> IO a
returnIO x = IO (\ s -> (# s, x #))

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO (\s -> case m s of (# new_s, x #) -> unIO (k x) new_s)

unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO x) = x

pattern Raw :: IO e -> L () IO e
pattern Raw e = Labeled e

module Main

import Control.ST
import Data.Bits

public export
interface RNG (m : Type -> Type) s a where
    seed : (sd : s) -> ST m Var [add (State s)]
    next : (x : Var) -> ST m a [x ::: State s]

[xorshiftplus] RNG m (Bits 64, Bits 64) (Bits 64) where
  seed s = new s
  next x = do (s0, s1) <- read x
              let a = s0 `xor` (s0 `shiftLeft` (cast 23))
              let b = ((a `xor` s1) 
                          `xor` (a `shiftRightLogical` (cast 17))) 
                          `xor` (s1 `shiftRightLogical` (cast 26))
              write x (s1, b) 
              pure (b `plus` s1)

main : IO ()
main = run $ 
  do putStr "Enter seed 1: "
     str1 <- getStr
     putStr "Enter seed 2: "
     str2 <- getStr
     s <- seed @{xorshiftplus} $ (intToBits $ cast str1, intToBits $ cast str2)
     putStrLn ("#1: " ++ (show $ bitsToInt !(next @{xorshiftplus} s)))
     putStrLn ("#2: " ++ (show $ bitsToInt !(next @{xorshiftplus} s)))
     putStrLn ("#3: " ++ (show $ bitsToInt !(next @{xorshiftplus} s)))
     delete s
    
{-# OPTIONS_GHC -O2 #-}
import Data.List
import Control.Monad

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x = all (\y -> x `rem` y /= 0) (2:[3,5..(floor $ sqrt $ fromIntegral x)])

data Fate = CENTRAL | LEFT | RIGHT | DEAD deriving Show

fate :: String -> Fate
fate sx | not ab = DEAD
        | cl && cr = CENTRAL
        | cr = RIGHT
        | cl = LEFT
        | otherwise = DEAD
    where   ab = isPrime x && (not $ '0' `elem` sx)
            c  = \fn -> all isPrime $ map read $ fn $ sx
            cl = c (init . tails)
            cr = c (tail . inits)
            x = read sx
          
main = do
    n <- (return . read) =<< getLine
    replicateM_ n ((print . fate) =<< getLine)
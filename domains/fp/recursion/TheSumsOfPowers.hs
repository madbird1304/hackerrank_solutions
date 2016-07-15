{-# OPTIONS_GHC -O2 #-}
import Control.Monad

subs :: Double -> Double -> [Double] -> Int
subs x m ls@(~(l:st)) | m == x = 1
					  | ls == [] || m > x = 0
					  | otherwise = subs x m st + subs x (l+m) st

ret x n = subs x 0 (map (**n) [1..x ** (1 / n)])

main = do 
	[x,n] <- (return . map read) =<< replicateM 2 getLine
	print $ ret x n
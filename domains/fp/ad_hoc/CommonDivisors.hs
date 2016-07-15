{-# OPTIONS_GHC -O2 #-}
import Control.Monad
primes :: [Integer]
primes = 1:2:[fromIntegral n | n <- [3,5..], all (\ x -> n`mod`x /= 0) (takeWhile (<sn n) (tail primes))] where
	sn k = floor $ sqrt $ fromIntegral k

factorize n = fac n (map fromIntegral $ tail primes) 1 where
	fac 1 _ t = t
	fac n (p:s) t = case divMod n p of
						(d,0) -> fac d (p:s) (t+1)
						_ -> t * fac n s 1


gcds [l,r] = factorize (gcd l r)
    

main = do
    (_:ds) <- (return . map (map read . words) . lines) =<< getContents
    mapM_ (print . gcds) ds

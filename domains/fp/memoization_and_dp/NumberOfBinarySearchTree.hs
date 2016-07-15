import Control.Monad
m = 10^8 + 7 :: Integer

catalan n = (product [1 .. 2*n]) `div` (((product [1..n])^2)*(n+1)) `mod` m

main = do
    m <- (return . read) =<< getLine
    replicateM_ m $ (print . catalan . read) =<< getLine
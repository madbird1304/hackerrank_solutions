fact n = product [1..n]
cnk n k = (fact n) `div` ((fact k) * (fact $ n-k))

main = do
    n <- getLine 
    ln <- sequence $ replicate (read n) getLine
    putStrLn . unlines $ map (show . (\[n,k] -> mod (cnk n k) (10^8 + 7)) . map read . words) ln 

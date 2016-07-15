import Data.List


makeBigNum m lst = foldl' fn 0 lst where
    fn l r = l + 200000 ^ (r - m)

recovList mi ma num = map fst $ filter ((/=0) . snd) $ map fn [0 .. ma - mi] where
    fn x = (x + mi , (num `mod` 200000 ^ (succ x)) `div` 200000 ^ x) 

main = do
    [_,a,_,b] <- return . map (map read . words) . lines =<< getContents
    let mi = min (minimum a) (minimum b)
        ma = max (maximum a) (maximum b)
        [b',a'] = map (makeBigNum mi) [b,a]
    putStrLn $ unwords $ map show $ recovList mi ma (b' - a')
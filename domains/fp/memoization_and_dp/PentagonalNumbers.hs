s n = n*(n + 1) `div` 2

p :: Integer -> Integer
p 1 = 1
p 2 = 5
p n = 3 * (s n) - 2 * n

main :: IO ()
main = do
    tl <- getLine
    let t = read tl
    il <- sequence . replicate t $ (getLine)
    let ret = map (show . p . read) il
    mapM_ putStrLn ret
    --return ()
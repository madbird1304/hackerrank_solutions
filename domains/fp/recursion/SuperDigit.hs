import Data.Char

--superDigit :: Integer -> String -> String
superDigit [d] = [d]
superDigit ds = superDigit $ show $ sum $ (map digitToInt ds)

sol :: String -> String -> String
sol n k = superDigit $ show $ (read k) * (sum $ map digitToInt n)

main :: IO ()
main = do
    [n,k] <- (return . words) =<< getLine
    putStrLn $ sol n k
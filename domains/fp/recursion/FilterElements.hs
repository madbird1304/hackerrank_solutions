{-# OPTIONS_GHC -O2 -funfolding-use-threshold1000 -funfolding-keeness-factor1000 #-}
import Data.List
import Control.Monad



proc [[n,k],lst] = fn lst remo where 
    remo = map head . filter ((>=k) . length) . group . sort $ lst
    {-# INLINE fn #-}
    fn _ [] = []
    fn (l:ls) rs = if l `elem` rs then l : r else r where r = fn ls (l `delete` rs)  
    

post [] = "-1"
post x = unwords $ map show x




main = do
    t <- (return . read) =<< getLine
    replicateM_ t $ (putStrLn . post . proc . (map $ map read . words)) =<< (replicateM 2 getLine)
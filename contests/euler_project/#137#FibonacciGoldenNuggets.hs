{-# OPTIONS_GHC -O2 -funfolding-use-threshold1000 -funfolding-keeness-factor1000 #-}
import Control.Monad (replicateM_)
import Data.Matrix
import Data.Word (Word64)


type T = Word64

{-# INLINE mo #-}
mo :: T
mo = 1000000007

{-# INLINE (.*) #-}
(.*) :: Matrix T -> Matrix T -> Matrix T
l .* r = fmap (`mod` mo) (l * r)



mv1 :: [Matrix T]
mv1 = map (fromList 3 1) [[2,5,1],[15,34,1],[104,233,1]]
mm3 :: Matrix T
mm3 = fromLists [[161,72,32],[360,161,72],[0,0,1]]
mmExp :: [Matrix T]
mmExp = mm3 : map (\ m -> m .* m) mmExp


{-# INLINE binary #-}
binary :: Integral t => t -> [Bool]
binary 0 = []
binary n = (m == 1) : binary d where (d,m) = divMod n 2


multi :: Integral t => t -> [Matrix T]
multi n = map snd . filter fst $ zip (binary n) mmExp



ret :: Integer -> T
ret n = (! (1,1)) $ foldr (.*) bases (multi d)
    where (d,m) = divMod (n-1) 3
          bases = mv1 !! (fromIntegral m)

main :: IO ()
main = do
    n <- (return . read) =<< getLine
    replicateM_ n ((print . ret . read) =<< getLine)

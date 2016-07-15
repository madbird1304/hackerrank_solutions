{-# OPTIONS_GHC -O2 #-}
import Control.Monad

{-# INLINE d #-}
d [x1,y1] [x2,y2] = sqrt $ (x1-x2)**2 + (y1-y2)**2

len f [l] = d f l 
len f (l:r:s) = d l r + len f (r:s)


main = do
    (_:p:ps) <- (return . map (map read . words) . lines) =<< getContents
    print $ len p (p:ps)
    
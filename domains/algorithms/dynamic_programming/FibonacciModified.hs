nth [a,_,1] = a
nth [a,b,n] = nth [b,(b^2 + a),(n-1)]

main = (print . nth . map read . words) =<< getLine
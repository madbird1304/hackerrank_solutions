import Control.Category

main = getLine >> getLine >>= (words >>> map read >>> foldl1 lcm >>> print)
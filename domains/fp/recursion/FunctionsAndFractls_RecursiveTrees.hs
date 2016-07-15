branchs _ 0 = []
branchs (x,y) n = [(x+n,y+n-1),(x-n,y+n-1)] ++ branchs (x,y) (n-1) 


iter 1 (x,y) h = (zip (repeat x) [y..y+h-1]) ++ branchs (x,y+h) h
iter n p@(x,y) h = (iter 1 p h) ++ iter (n-1) (x+h,y+2*h) (h `div` 2) ++ iter (n-1) (x-h,y+2*h) (h `div` 2)


put i lst v = take i lst ++ [v] ++ drop (i+1) lst

paint (x,y) lst = put y lst (put x (lst !! y) '1')

strTree n = foldr paint (replicate 63 (replicate 100 '_')) (iter n (49,0) 16)


main :: IO ()
main = do
	inp <- getLine
	let n = read inp
	putStr $ unlines $ reverse $ strTree n
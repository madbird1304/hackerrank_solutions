module Main where



func (r,g,b,y) (s:sx) | abs (r - g)> 1 || abs (y - b) > 1 = False
					  | otherwise = case s of
                                        'R' -> func (succ r,g,b,y) sx
                                        'G' -> func (r,succ g,b,y) sx
                                        'B' -> func (r,g,succ b,y) sx
                                        'Y' -> func (r,g,b,succ y) sx
                                        _ -> func (r,g,b,succ y) sx
func (r,g,b,y) [] = r == g && b == y

main = do
    (n:inp) <- (return . lines) =<< getContents
    mapM_ (print . func (0,0,0,0)) inp 
	
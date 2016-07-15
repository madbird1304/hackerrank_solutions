import Data.List
import Control.Monad

data BTree = Empty | Node String BTree BTree

ins Empty str = Node str Empty Empty
ins (Node str' l r) str | str < str' = Node str' (ins l str) r
                        | otherwise  = Node str' l (ins r str)


el Empty str = False
el (Node str' l r) str = str' == str || ((str < str') && (el l str)) || el r str


check tree st [] ret = if el tree st then st : ret else []
check tree st (s:tr) ret | el tree st = check tree [s] tr (st:ret)
                         | otherwise = check tree (s:st) tr ret

main :: IO ()
main = do
    n <- (return . read) =<< getLine
    replicateM_ n $ do
        _ <- getLine
        wds <- (return . words) =<< getLine
        (s:tr) <- getLine
        let tree = foldl' ins Empty $ map reverse wds
        putStrLn $ case (check tree [s] tr []) of
            [] -> "WRONG PASSWORD"
            ret -> unwords . map reverse . reverse $ ret
                


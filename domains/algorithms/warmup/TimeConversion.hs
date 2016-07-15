import Data.List
import Control.Monad

parse str = concat $ intersperse ":" $ proc chunks where
    proc ["12",mm,ss,"AM"] = ["00",mm,ss]
    proc a@["12"  ,mm,ss,"PM"] = init a
    proc [hh  ,mm,ss,"PM"] = [(show . (+12) . read $ hh),mm,ss]
    proc a = init a 
    chunks = map (\i -> drop i . take (i+2) $ raw) [0,2,4,6]
    raw = filter (/=':') str


main = getLine >>= (return . parse) >>= putStrLn
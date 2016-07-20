
{-

>   Increment data pointer so that it points to next location in memory.

<   Decrement data pointer so that it points to previous locaion in memory.

+   Increment the byte pointed by data pointer by 1. If it is already at its maximum value, 255, then new value will be 0.

-   Decrement the byte pointed by data pointer by 1. If it is at its minimum value, 0, then new value will be 255.

.   Output the character represented by the byte at the data pointer.

,   Read one byte and store it at the memory location pointed by data pointer.

[   If the byte pointed by data pointer is zero, then move instruction pointer to next matching ']', otherwise move instruction pointer to next command.

]   If the byte pointed by data pointer is non-zero, then move instruction pointer to previous matching '[' command, otherwise to next command.

-}
import Data.List
import Data.Ord
import Data.Char
import Data.Word
import Control.Monad.State


--type Code = [(Int,Char)]
data Machine = Machine {
                        data_ptr :: Int,
                        memory :: [Word8],
                        code_ptr :: Int,
                        cycle_jumps :: [(Int,Int)],
                        output :: String,
                        code :: [(Int,Char)],
                        input :: String,
                        ticks :: Integer,
                        journal :: [Int]
                       }

origin = Machine {
                  data_ptr = 0,
                  memory = repeat 0,
                  code_ptr = 0,
                  output = "",
                  ticks = 1,
                  journal = []
                 }


recognize :: Char -> State Machine ()
recognize '>' = incDataPtr >> incCodePtr
recognize '<' = decDataPtr >> incCodePtr
recognize '+' = incDataCell >> incCodePtr
recognize '-' = decDataCell >> incCodePtr
recognize '.' = printDataCell >> incCodePtr
recognize ',' = readInput >> incCodePtr
recognize '[' = beginCycle
recognize ']' = endCycle 
recognize _ = incCodePtr


mainCycle :: State Machine String
mainCycle = do
  m@(Machine {code_ptr = cp, code = cs, output = op, ticks = t}) <- get
  case lookup cp cs of
      Nothing -> return $ reverse op
      Just oper -> do
          case t > 10^5 of
              True -> return $ reverse op ++ "\nPROCESS TIME OUT. KILLED!!!"
              False -> do
                  let action = recognize oper
                  action >> tick >> mainCycle
                  
tick :: State Machine ()
tick = do
  m@(Machine {ticks = t, journal = j, code_ptr = cp}) <- get
  put $ m {ticks = succ t, journal = cp:j}

incCodePtr :: State Machine ()
incCodePtr = do
  m@(Machine {code_ptr = cp}) <- get
  put $ m {code_ptr = succ cp}

beginCycle :: State Machine ()
beginCycle = do
    m@(Machine {data_ptr = dp, memory = mem, cycle_jumps = cj, code_ptr = cp}) <- get
    case mem !! dp of
        0 -> do
            let Just new_cp = lookup cp cj
            put $ m {code_ptr = new_cp}
        _ -> incCodePtr

endCycle :: State Machine ()
endCycle = do
    m@(Machine {data_ptr = dp, memory = mem, cycle_jumps = cj, code_ptr = cp}) <- get
    case mem !! dp of
        0 -> incCodePtr
        _ -> do
            let Just new_cp = lookup cp cj
            put $ m {code_ptr = new_cp}




readInput :: State Machine ()
readInput = do
    m@(Machine {memory = mem, data_ptr = dp, input = (i:p)}) <- get
    let (l,cell:r) = splitAt dp mem
        mem' = l ++ (fromIntegral $ ord i):r
    put $ m {memory = mem', input = p}

printDataCell :: State Machine ()
printDataCell = do
    m@(Machine {memory = mem, data_ptr = dp, output = op}) <- get
    put $ m {output = (chr $ fromIntegral $ mem !! dp):op} 

decDataCell :: State Machine ()
decDataCell = do
    m@(Machine {memory = mem, data_ptr = dp}) <- get
    let (l,cell:r) = splitAt dp mem
        mem' = l ++ (cell - 1):r
    put $ m {memory = mem'}

incDataCell :: State Machine ()
incDataCell = do
    m@(Machine {memory = mem, data_ptr = dp}) <- get
    let (l,cell:r) = splitAt dp mem
        mem' = l ++ (cell + 1):r
    put $ m {memory = mem'}

incDataPtr :: State Machine ()
incDataPtr = do
    m@(Machine {data_ptr = dp}) <- get
    put $ m {data_ptr = (succ dp)}

decDataPtr :: State Machine ()
decDataPtr = do
    m@(Machine {data_ptr = dp}) <- get
    put $ m {data_ptr = (pred dp)}

genCycleJmps :: [(Int,Char)] -> [(Int,Int)]
genCycleJmps = findmb [] where
    findmb _ [] = []
    findmb unclosed@(~(u:nclosed)) (c:ode) | snd c == ']' = [(fst u,fst c),(fst c,fst u)] ++ findmb nclosed ode
                                           | snd c == '[' = findmb (c:unclosed) ode
                                           | otherwise    = findmb (u:nclosed) ode


ops = "<>+-.,[]"

main :: IO ()
main = do
    inp <- getLine >> getLine
    code <- getContents
    let numb_code = zip [0..] (filter (`elem` ops) code)
        cycle_j = genCycleJmps numb_code
        machine = origin {code = numb_code, cycle_jumps = cycle_j, input = init inp}
        (res,machine') = runState mainCycle machine
    --print $ reverse $ journal machine'
    putStrLn res

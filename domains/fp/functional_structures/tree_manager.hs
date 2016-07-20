{-# OPTIONS_GHC -O2 #-}
import Data.HashMap hiding (map)
import Control.Monad.State
import System.IO.Unsafe
import Data.List (foldl1')

type Idx = Int

data Node = Deleted | Node {valIdx :: Idx, childrenIdx :: [Idx]} deriving (Eq,Show)

type MapTree = Map Idx Node
type MapVals = Map Idx Int

data World = World {trees :: MapTree, vals :: MapVals, root :: Idx, current :: [Idx], indexer :: Idx, out :: [Int]} deriving (Eq,Show)

origin = World {trees = singleton 0 (Node 0 []), vals = singleton 0 0, root = 0, current = [], indexer = 0, out = []} 


mDelete :: State World ()
mDelete = do
  World {current = (curInPar:_)} <- get
  mVisitParent
  parIdx <- mCurrentIdx
  w@(World {trees = ts}) <- get
  let (Node vi ci) = ts ! parIdx
      ci' = take curInPar ci ++ drop (curInPar + 1) ci
      ts' = insert parIdx (Node vi ci') ts
  put $ w {trees = ts'}
      
mInsertLeft :: Int -> State World ()
mInsertLeft x = do
    World {current = (c:ur)} <- get
    mVisitParent
    i <- mCurrentIdx
    newNIdx <- next
    newVIdx <- next
    w@(World {trees = ts, vals = vs}) <- get
    let (Node vi ci) = ts ! i
        (lp,rp) = splitAt c ci
        ts'  = insert i (Node vi (lp ++ [newNIdx] ++ rp)) ts
        ts'' = insert newNIdx (Node newVIdx []) ts'
        vs' = insert newVIdx x vs
    put $ w {trees = ts'', vals = vs'}
    mVisitChild (c+1)

mInsertRight :: Int -> State World ()
mInsertRight x = do
    World {current = (c:ur)} <- get
    mVisitParent
    i <- mCurrentIdx
    newNIdx <- next
    newVIdx <- next
    w@(World {trees = ts, vals = vs}) <- get
    let (Node vi ci) = ts ! i
        (lp,rp) = splitAt (c+1) ci
        ts'  = insert i (Node vi (lp ++ [newNIdx] ++ rp)) ts
        ts'' = insert newNIdx (Node newVIdx []) ts'
        vs' = insert newVIdx x vs
    put $ w {trees = ts'', vals = vs'}
    mVisitChild c

mPrint :: State World ()
mPrint = do
    w@(World {vals = vs, trees = ts, out = o}) <- get
    cur <- mCurrentIdx
    let val = vs ! (valIdx (ts ! cur))
    put $ w {out = val:o}

mInsertChild :: Int -> State World ()
mInsertChild x = do
    i <- mCurrentIdx
    newNIdx <- next
    newVIdx <- next
    w@(World {trees = ts, vals = vs}) <- get
    let (Node vi ci) = ts ! i
        ts'  = insert i (Node vi (newNIdx:ci)) ts
        ts'' = insert newNIdx (Node newVIdx []) ts'
        vs' = insert newVIdx x vs
    put $ w {trees = ts'', vals = vs'}


 


mVisitChild :: Int -> State World ()
mVisitChild n = do
    wld <- get
    put $ wld {current = n : current wld}

mVisitParent :: State World ()
mVisitParent = do
    wld <- get
    put $ wld {current = tail $ current wld}

mVisitLeft :: State World ()
mVisitLeft = do
    wld <- get
    let (c:cs) = current wld
    put $ wld { current = (pred c):cs}

mVisitRight :: State World ()
mVisitRight = do
    wld <- get
    let (c:cs) = current wld
    put $ wld { current = (succ c):cs}


mChangeValue :: Int -> State World ()
mChangeValue x = do
    tidx <- mCurrentIdx
    w@(World {vals = vs, trees = ts}) <- get
    let vs' = insert (valIdx (ts ! tidx)) x vs
    put $ w {vals = vs'}

mCurrentIdx :: State World Idx
mCurrentIdx = do
    World {current = cur, trees = ts, root = rt} <- get
    return $ foldr (\newi curi -> (childrenIdx (ts ! curi)) !! newi) rt cur



next :: State World Idx
next = do
    w@(World {indexer = i}) <- get
    put $ w {indexer = succ i}
    return $ succ i

parse :: [String] -> State World ()
parse ["change",x] = mChangeValue (read x)
parse ["print"] = mPrint
parse ["visit","left"] = mVisitLeft
parse ["visit","right"] = mVisitRight
parse ["visit","parent"] = mVisitParent
parse ["visit","child",n] = mVisitChild (pred $ read n)
parse ["insert","left",x] = mInsertLeft (read x)
parse ["insert","right",x] = mInsertRight (read x)
parse ["insert","child",x] = mInsertChild (read x)
parse ["delete"] = mDelete
parse _ = return ()

main = do
  strs <- getContents
  let commands = map (parse . words) . tail . lines $ strs
      result = reverse $ out $ execState (foldl1' (>>) commands) origin
  mapM_ print result
      

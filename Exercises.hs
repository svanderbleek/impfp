{-# LANGUAGE NoImplicitPrelude #-}

module Exercises where

import Control.Monad.State.Lazy
  (execState
  ,evalState
  ,runState
  ,put
  ,get
  ,forM_
  ,modify
  ,return
  ,State)

import Data.Map
  (Map
  ,empty
  ,insert
  ,lookup
  ,(!))

import Prelude
  (Int
  ,Double
  ,(+)
  ,(-)
  ,(/)
  ,(*)
  ,($)
  ,(<$>)
  ,(<*>)
  ,(>>)
  ,flip
  ,fst
  ,sqrt
  ,undefined
  ,Ord
  ,Eq
  ,Show
  ,Maybe(..))

-- | Canonize
--
-- source: https://cseweb.ucsd.edu/classes/wi13/cse230-a/lectures/monads2.html
--
-- >>> canonize ["zebra", "mouse", "zebra", "zebra", "owl"]
-- [0,1,0,0,2]

canonize :: Ord a => [a] -> [Int]
canonize as =
  let (s, _) = execState (canonizeWithStoreAndCounter as) (empty, 0) in
  (\a -> s ! a) <$> as

canonizeWithStoreAndCounter :: Ord a => [a] -> State (Map a Int, Int) ()
canonizeWithStoreAndCounter [] =
  return ()
canonizeWithStoreAndCounter (a:as) = do
  (s, c) <- get
  case lookup a s of
    Nothing ->
      modify $ \(s, c) -> (insert a c s, c+1)
    _ -> return ()
  canonizeWithStoreAndCounter as


-- | Leaf Label
--
-- source: https://cseweb.ucsd.edu/classes/wi13/cse230-a/lectures/monads2.html
--
-- >>> leafLabel (Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c'))
-- Node (Node (Leaf ('a',0)) (Leaf ('b',1))) (Leaf ('c',2))

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Eq, Show)

leafLabel :: Tree a -> Tree (a, Int)
leafLabel t =
  evalState (labelWithCounter t) 0

labelWithCounter :: Tree a -> State Int (Tree (a, Int))
labelWithCounter (Node l r) =
  Node <$> (labelWithCounter l) <*> (labelWithCounter r)
labelWithCounter (Leaf a) = do
  c <- get
  modify (+1)
  return $ Leaf (a, c)

-- | CalcState
--
-- source: http://cmsc-16100.cs.uchicago.edu/2016/Lectures/18-state-monad-1.php
--
-- >>> :{
--  calc $ do
--   push 1
--   push 2
--   add
--   push 3
--   mul
-- :}
-- 9.0

type CalcState
  = State [Double]

type Calculation
  = CalcState ()

pop :: CalcState Double
pop = do
  stack <- get
  case stack of
    [] -> return 0.0
    x:xs -> do
      put xs
      return x

push :: Double -> CalcState ()
push d = do
  stack <- get
  put (d:stack)

binOp :: (Double -> Double -> Double) -> CalcState ()
binOp f = do
  x <- pop
  y <- pop
  push $ f x y

add, sub, mul, div :: Calculation
add =
  binOp (+)
sub =
  binOp (-)
mul =
  binOp (*)
div =
  binOp (/)

calc :: Calculation -> Double
calc c =
  evalState (c >> pop) []

swap :: Calculation
swap = do
  x <- pop
  y <- pop
  push x
  push y

dup :: Calculation
dup = do
  x <- pop
  push x
  push x

unOp :: (Double -> Double) -> CalcState ()
unOp f = do
  x <- pop
  push $ f x

sqrtc :: Calculation
sqrtc =
  unOp sqrt

sqrc :: Calculation
sqrc = do
  dup
  mul

-- | store / recall
--
-- modify state to have memory cell
-- store and recall top of stack to this cell

data InternalState
  = InternalState
  { stack :: [Double]
  , memory :: Double }

-- type CalcState = State InternalState

store :: Calculation
store =
  undefined

recall :: Calculation
recall =
  undefined

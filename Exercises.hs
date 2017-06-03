{-# LANGUAGE NoImplicitPrelude #-}

module Exercises where

import Control.Monad.State.Lazy
  (execState
  ,evalState
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
  ,(+)
  ,($)
  ,(<$>)
  ,(<*>)
  ,flip
  ,undefined ,Ord
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



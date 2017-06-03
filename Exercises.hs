{-# LANGUAGE NoImplicitPrelude #-}

module Exercises where

import Control.Monad.State.Lazy
  (execState
  ,put
  ,get
  ,forM_
  ,modify
  ,return)

import Data.Map
  (Map
  ,empty
  ,insert
  ,lookup
  ,(!))

import Data.Maybe
  (Maybe(..))

import Data.Ord
  (Ord)

import Prelude
  (Int
  ,(+)
  ,($)
  ,(<$>)
  ,flip)

-- | Canonize
--
-- source: https://cseweb.ucsd.edu/classes/wi13/cse230-a/lectures/monads2.html
--
-- >>> canonize ["zebra", "mouse", "zebra", "zebra", "owl"]
-- [0,1,0,0,2]

canonize :: Ord a => [a] -> [Int]
canonize as =
  let
    (s, _) = (flip execState) (empty, 0) $ do
      forM_ as (\a -> do
          (s, c) <- get
          case lookup a s of
            Nothing -> modify (\(s, c) ->
              (insert a c s, c + 1))
            _ -> return ())
  in
  (\a -> s ! a) <$> as

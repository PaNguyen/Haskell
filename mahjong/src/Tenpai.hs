module Tenpai(findTenpai, isTenpai, isComplete) where

import Data.List
import Data.Maybe
import Control.Applicative

import Debug.Trace

import Tiles

isComplete = undefined
-- isComplete :: Hand -> Bool
-- isComplete (Hand hand) = isCompleteHelp hand []
--   where isCompleteHelp hand unused = 
--           case hand of
--             [] ->
--               (case unused of
--                  [x,y] | x == y -> True
--                  _ -> False
--               )
--             x:xs ->
--               (case tryMakeRun x xs of -- try use as run
--                  Just (set, rest) -> isCompleteHelp rest unused
--                  _ -> False
--               ) ||
--               (case tryMakeTrip x xs of --try use as trip
--                  Just (set, rest) -> isCompleteHelp rest unused
--                  _ -> False
--               ) ||
--               (isCompleteHelp hand (x:unused)
--               )

isTenpai :: Hand -> Bool
isTenpai (Hand hand) = findTenpai hand /= Nothing && length hand == 13

--no kokushi or chitoitsu
findTenpai :: [Tile] -> Maybe Hand
findTenpai drawn = evalTenpai 4 drawn
  where evalTenpai _ [] = Just $ Hand []
        evalTenpai numSets drawn = let
          sorted = sort drawn
          in findPairTenpai numSets sorted [] <|> findTankiTenpai numSets sorted [] 

findTankiTenpai :: Int -> [Tile] -> [Tile] -> Maybe Hand
findTankiTenpai _ [] _ = Nothing
findTankiTenpai 0 (x:xs) unused = Just . Hand . (:[]) . head $ reverse unused ++ [x] ++ xs
findTankiTenpai numSets (x:xs) unused =
  (do
      (set, rest) <- tryMakeRun x xs
      Hand hand <- findTankiTenpai (numSets - 1) rest unused
      return . Hand $ set ++ hand
  ) <|>
  (do
      (set, rest) <- tryMakeTrip x xs
      Hand hand <- findTankiTenpai (numSets - 1) rest unused
      return . Hand $ set ++ hand
  ) <|>
  (do
      findTankiTenpai numSets xs (x:unused)
  )

findPairTenpai :: Int -> [Tile] -> [Tile] -> Maybe Hand
findPairTenpai _ [] _ = Nothing
findPairTenpai _ [x] _ = Nothing
findPairTenpai 1 xs unused = do
  let rest = reverse unused ++ xs 
  (wait, restFinal) <- findKanchan rest <|> findPenchan rest <|> findRyanmen rest <|> findPair rest
  (pair, _) <- findPair restFinal
  return . Hand $ wait ++ pair
findPairTenpai numSets (x:xs) unused =
  (do
      (set, rest) <- tryMakeRun x xs
      Hand hand <- findPairTenpai (numSets - 1) rest unused
      return . Hand $ set ++ hand
  ) <|>
  (do
      (set, rest) <- tryMakeTrip x xs
      Hand hand <- findPairTenpai (numSets - 1) rest unused
      return . Hand $ set ++ hand
  ) <|>
  (do
      findPairTenpai numSets xs (x:unused)
  )

-- x:y:zs is sorted
-- returns (pair of x, rest of hand)
tryMakePair x (y:zs) | x == y = Just ([x,y], zs)
tryMakePair _ _ = Nothing

-- x:y:z:zs is sorted
-- returns (triplet of x, rest of hand
tryMakeTrip x (y:z:xs) | x == y && x == z = Just ([x,y,z], xs)
tryMakeTrip _ _ = Nothing

-- x:drawn is sorted
-- returns (set starting with x, rest of hand), with set being
-- 3 distinct tiles which fulfill cond
tryMakeRun x drawn = go x drawn []
  where
    -- go matches x with consecutive y and z, having to deal with duplicates
    go x [] _ = Nothing
    go x (y:ys) rest | x == y = go x ys (y:rest)
    go x (y:ys) rest = goMore x y ys rest
    -- goMore matches x and y with consecutive z, having to deal with duplicates
    goMore x y [] _ = Nothing
    goMore x y (z:zs) rest | z == y = goMore x y zs (z:rest)
    goMore x y (z:zs) rest = if good then Just([x,y,z], reverse rest ++ zs) else Nothing
      where good = 
              case (x,y,z) of
                (Bamboo x', Bamboo y', Bamboo z') | y' == x'+1 && z' == y'+1 -> True
                (Character x', Character y', Character z') | y' == x'+1 && z' == y'+1 -> True
                (Pin x', Pin y', Pin z') | y' == x'+1 && z' == y'+1 -> True
                _ -> False
  
-- x:drawn is sorted
-- returns (set starting with x, rest of hand), with set being
-- 2 distinct tiles which fulfill cond
tryMakeCond2 cond x drawn = go x drawn []
  where
    -- go matches x with consecutive y and z, having to deal with duplicates
    go x [] _ = Nothing
    go x (y:ys) rest | x == y = go x ys (y:rest)
    go x (y:ys) rest = if cond x y then Just([x,y], reverse rest ++ ys) else Nothing

tryMakeKanchan = tryMakeCond2 isKanchan
tryMakePenchan = tryMakeCond2 isPenchan
tryMakeRyanmen = tryMakeCond2 isRyanmen

findWait :: (Tile -> [Tile] -> Maybe ([Tile], [Tile])) -> [Tile] -> Maybe ([Tile], [Tile])
findWait tryMake [] = Nothing
findWait tryMake (x:xs) = tryMake x xs <|> (findWait tryMake xs >>= \(wait, rest) -> Just (wait, x:rest))

findPair = findWait tryMakePair
findKanchan = findWait tryMakeKanchan
findPenchan = findWait tryMakePenchan
findRyanmen = findWait tryMakeRyanmen


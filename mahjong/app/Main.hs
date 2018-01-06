module Main where

import Debug.Trace
import Lib
import Tenpai
import Tiles
import Data.List
import System.Random
import Data.Array.IO
import Control.Monad



-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs


prefixLetter t =
  case t of
    Bamboo _ -> "S"
    Pin _ -> "P"
    Character _ -> "M"
    _ -> ""

singleLetter t =
  case t of
    Bamboo i -> show i
    Pin i -> show i
    Character i -> show i
    Dragon d -> (:[]) . head $ show d
    Wind w -> (:[]) . head $ show w


pretty hand =
  let grouped = groupBy sameSuit hand
      sorted = map sort grouped
      prefix = map (\g -> prefixLetter . head $ nubBy (\_ _ -> True) g) sorted
      zipped = zip (map (map singleLetter) sorted) prefix
  in concatMap (\(l,p) -> concat l ++ p) zipped

getTenpaiPrefix :: Deck -> Hand
getTenpaiPrefix (Deck deck) = evalTenpai deck []
  where
    evalTenpai [] drawn = undefined -- should never happen
    evalTenpai (x:xs) drawn = 
      case findTenpai (x:drawn) of
        Just hand -> hand
        Nothing -> evalTenpai xs (x:drawn)


main :: IO ()
main = do
  -- tiles <- shuffle $ tiles ++ tiles ++ tiles ++ tiles
  allowed <- return $ [Dragon White] ++ pins
  tiles <- shuffle $ allowed ++ allowed ++ allowed ++ allowed
  -- putStrLn $ show tiles
  let deck = Deck tiles
  Hand tenpai <- return $ getTenpaiPrefix deck
  putStrLn $ show $ pretty  $ sort tenpai
  putStrLn $ show $ waitingTiles $ Hand tenpai
  let hand = [Pin 4, Pin 4, Pin 5, Pin 5, Pin 6, Pin 6, Pin 8, Pin 8, Pin 8, Pin 8, Pin 9, Pin 9, Pin 9]
  putStrLn . show . isComplete $ Hand hand
  return ()

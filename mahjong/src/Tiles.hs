module Tiles where

data Dragon = Red | White | Green deriving (Eq, Ord, Show)
data Wind = North | East | South | West deriving (Eq, Ord, Show)
data Tile = Dragon Dragon | Wind Wind | Bamboo Int | Character Int | Pin Int deriving (Eq, Ord, Show)
newtype Deck = Deck [Tile] deriving Show
newtype Hand = Hand [Tile] deriving (Show, Eq)


dragons = map Dragon [Red, White, Green]

winds = map Wind [North, East, South, West]

bamboos = map Bamboo  [1..9]

characters = map Character [1..9]

pins = map Pin  [1..9]

tiles = dragons ++ winds ++ bamboos ++ characters ++ pins

sameSuit x y =
  case (x,y) of
    (Dragon _, Dragon _) -> True
    (Wind _, Wind _) -> True
    (Bamboo _, Bamboo _) -> True
    (Pin _, Pin _) -> True
    (Character _, Character _) -> True
    _ -> False


isKanchan x y =
  case (x, y) of
    (Bamboo x', Bamboo y') | y' == x'+2 -> True
    (Pin x', Pin y') | y' == x'+2 -> True
    (Character x', Character y') | y' == x'+2 -> True
    _ -> False

isPenchan x y =
  case (x,y) of
    (Bamboo 1, Bamboo 2) -> True
    (Pin 1, Pin 2) -> True
    (Character 1, Character 1) -> True
    (Bamboo 8, Bamboo 9) -> True
    (Pin 8, Pin 9) -> True
    (Character 8, Character 9) -> True
    _ -> False

isRyanmen x y = not (isPenchan x y) &&
  case (x, y) of
    (Bamboo x', Bamboo y') | y' == x'+1 -> True
    (Pin x', Pin y') | y' == x'+1 -> True
    (Character x', Character y') | y' == x'+1 -> True
    _ -> False

{-# LANGUAGE RecordWildCards #-}

module MinesweeperLib (BoardState (..), Tile (..), Result (..), MoveType (..), getResult, alterBoard) where

import Data.Char
import Data.List (intercalate, nub)
import Debug.Trace

type Tile = (Int, Int)

data BoardState = BoardState {h :: Int, w :: Int, mines :: [Tile], flags :: [Tile], guesses :: [Tile]}

instance Show BoardState where
  show BoardState {..} = hEnum ++ hBar ++ board ++ hBar ++ hEnum
    where
      posBoard = [[(y, x) :: Tile | x <- [0 .. w -1]] | y <- [0 .. h -1]]
      hBar = " |" ++ (take w (repeat '-')) ++ "| \n"
      hEnum = "  " ++ take (w) charEnumeration ++ " \n"
      board = concat (zipWith (\i cs -> [i] ++ "|" ++ cs ++ "|" ++ [i] ++ "\n") charEnumeration ((map . map) ((charAtTile BoardState {..})) (posBoard)))

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

charToString :: Char -> String
charToString = (: [])

charEnumeration :: String
charEnumeration = map intToDigit [0 .. 9] ++ ['A' .. 'Z']

data Result = Ongoing | Win | Loss
  deriving (Show)

data MoveType = Flag | Guess

alterBoard :: MoveType -> BoardState -> Tile -> BoardState
alterBoard Flag = placeFlag
alterBoard Guess = placeGuess

getResult :: BoardState -> Result
getResult BoardState {..}
  | any (`elem` mines) guesses = Loss
  | guessedAllGivenNoLoss BoardState {..} = Win
  | otherwise = Ongoing

guessedAllGivenNoLoss :: BoardState -> Bool
guessedAllGivenNoLoss BoardState {..} = length mines + length (getOpen BoardState {..}) == w * h

placeFlag :: BoardState -> Tile -> BoardState
placeFlag bs tile
  | isFlag bs tile || isOpen bs tile = bs
  | otherwise = BoardState (h bs) (w bs) (mines bs) (tile : flags bs) (guesses bs)

placeGuess :: BoardState -> Tile -> BoardState
placeGuess bs tile
  | isFlag bs tile || isOpen bs tile = bs
  | otherwise = BoardState (h bs) (w bs) (mines bs) (flags bs) (tile : guesses bs)

isFlag :: BoardState -> Tile -> Bool
isFlag BoardState {..} tile = tile `elem` flags

isOpen :: BoardState -> Tile -> Bool
isOpen bs tile = elem tile $ getOpen bs

getOpen :: BoardState -> [Tile]
getOpen BoardState {..} = foldl (\found guess -> star BoardState {..} guess found guess) [] guesses

star :: BoardState -> Tile -> [Tile] -> Tile -> [Tile]
star BoardState {..} start found tile
  | (mineCount BoardState {..} tile == 0 && notElem tile found) = foldl (star BoardState {..} start) (tile : found) (filter (isValidTile h w) $ map (tupleSum tile) directions8)
  | elem tile found = found
  | otherwise = tile : found

directions8 :: [(Int, Int)]
directions8 = [(y, x) | x <- d, y <- d, x /= y || x /= 0]
  where
    d = [-1, 0, 1]

directions4 :: [(Int, Int)]
directions4 = [(0, -1), (0, 1), (-1, 0), (1, 0)]

tupleSum :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
tupleSum (a, b) (a', b') = (a + a', b + b')

isValidTile :: Int -> Int -> Tile -> Bool
isValidTile h w (y, x) = y >= 0 && y < h && x >= 0 && x < w

isMine :: BoardState -> Tile -> Bool
isMine BoardState {..} tile = tile `elem` mines

mineCount :: BoardState -> Tile -> Int
mineCount bs tile = length . (filter (isMine bs)) . (filter (isValidTile (h bs) (w bs))) . map (tupleSum tile) $ directions8

charAtTile :: BoardState -> Tile -> Char
charAtTile bs tile
  | open = if n == 0 then '.' else intToDigit n
  | isFlag bs tile = '!'
  | otherwise = ' '
  where
    n = mineCount bs tile
    open = isOpen bs tile

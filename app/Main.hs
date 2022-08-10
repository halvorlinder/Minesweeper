module Main where

import Data.Char (isAlpha, isNumber, ord, toLower)
import MinesweeperLib (BoardState (..), MoveType (..), Result (..), Tile (..), alterBoard, getResult)
import Parse (coordinatePred, coordinateRead)
import System.Random (Random (random), randomIO)

maxSize :: Int
maxSize = 32

main :: IO ()
main = do
  putStrLn "Width:"
  r <- getBoardSizeCoord
  putStrLn "Height"
  c <- getBoardSizeCoord
  board <- createBoard r c
  game board
  where
    getBoardSizeCoord = getInt ("You need to supply a number between 0 and " ++ show maxSize) (read :: (String -> Int)) (all isNumber) 0 maxSize

game :: BoardState -> IO ()
game state = do
  clearScreen
  print state
  let res = getResult state
  case res of
    Ongoing -> do
      mt <- getMoveType
      tile <- getTile (h state) (w state)
      game (alterBoard mt state tile)
    _ -> do gameOver res

getSingleChar :: String -> (Char -> Bool) -> IO Char
getSingleChar errorMessage pred = do
  l <- getLine
  if length l <= 2 && length l > 0 && pred (head l)
    then return (head l)
    else do
      putStrLn errorMessage
      getSingleChar errorMessage pred

getInt :: String -> (String -> Int) -> (String -> Bool) -> Int -> Int -> IO Int
getInt message translate pred low high = do
  nstr <- getLine
  if not (null nstr) && pred nstr
    then do
      let n = translate nstr :: Int
      if n >= low && n <= high
        then do return n
        else do
          putStrLn message
          getInt message translate pred low high
    else do
      putStrLn message
      getInt message translate pred low high

getMoveType :: IO MoveType
getMoveType = do
  putStrLn "Place or remove a Flag (F) or Guess a safe square (G)"
  c <- getSingleChar "Invalid input, supply a single character (F|G)" (`elem` "FG")
  case c of
    'F' -> do return Flag
    'G' -> do return Guess
    _ -> error "Could not determine MoveType"

getTile :: Int -> Int -> IO Tile
getTile h w = do
  putStrLn "Y coordinate"
  r <- getCoord (h -1)
  putStrLn "X coordinate"
  c <- getCoord (w -1)
  return (r, c)
  where
    getCoord = getInt "You need to supply a coordinate within the board!" coordinateRead coordinatePred 0

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

createBoard :: Int -> Int -> IO BoardState
createBoard h w = do
  bs <- randomBombs h w
  return (BoardState h w bs [] [])

randomBombs :: Int -> Int -> IO [Tile]
randomBombs w h = do
  rs <- randomInts (div (w * h) 10) 0 (w * h)
  return $ map (\n -> (div n w, mod n w)) rs

randomInts :: Int -> Int -> Int -> IO [Int]
randomInts = randomIntsInner []

randomIntsInner :: [Int] -> Int -> Int -> Int -> IO [Int]
randomIntsInner rs n low high = do
  if length rs == n
    then return rs
    else do
      r <- randomInt low high
      if r `elem` rs
        then
          ( do
              randomIntsInner rs n low high
          )
        else
          ( do
              randomIntsInner (r : rs) n low high
          )

randomInt :: Int -> Int -> IO Int
randomInt low high = do
  num <- randomIO :: IO Float
  return $ floor (num * fromIntegral (high - low)) + low

gameOver :: Result -> IO ()
gameOver Win = putStrLn "Win"
gameOver Loss = putStrLn "Loss"
gameOver _ = error "Gameover called with Ongoing"

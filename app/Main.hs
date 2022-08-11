{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Char (isAlpha, isNumber, ord, toLower, toUpper)
import MinesweeperLib (BoardState (..), MoveType (..), Result (..), Tile (..), alterBoard, getResult, isValidTile, tupleSum)
import Parse (coordinatePred, coordinateRead)
import System.Random (Random (random), randomIO)

data Mode = Coordinate | Cursor Tile

data Command = ModeToggle | CursorMove Tile | Move MoveType Tile

data GameState = GameState {mode :: Mode, bs :: BoardState}

instance Show GameState where
  show (GameState (Cursor (h', w')) bs) = take realPos boardStr ++ blueBack ++ [boardStr !! realPos] ++ noBack ++ drop (realPos + 1) boardStr
    where
      cursorPos = w bs * h' + w'
      boardStr = show bs
      startOffset = 11 + 2 * w bs
      betweenOffset = 5
      realPos = startOffset + cursorPos + betweenOffset * h'
  show (GameState Coordinate bs) = show bs

blueBack :: [Char]
blueBack = "\ESC[48;5;57m"

noBack :: [Char]
noBack = "\ESC[49m"

maxSize :: Int
maxSize = 32

main :: IO ()
main = do
  putStrLn "Width:"
  r <- getBoardSizeCoord
  putStrLn "Height"
  c <- getBoardSizeCoord
  board <- createBoard r c
  game (GameState Coordinate board)
  where
    getBoardSizeCoord = getInt ("You need to supply a number between 0 and " ++ show maxSize) (read :: (String -> Int)) (all isNumber) 0 maxSize

doCommand :: Command -> GameState -> GameState
doCommand ModeToggle = toggleMode
doCommand (CursorMove tile) = moveCursor tile
doCommand (Move moveType tile) = doMove moveType tile

doMove :: MoveType -> Tile -> GameState -> GameState
doMove moveType tile GameState {..} = GameState mode (alterBoard moveType bs tile)

moveCursor :: Tile -> GameState -> GameState
moveCursor _ (GameState Coordinate _) = error "Cannot move cursor in Coordinate mode"
moveCursor moveTile (GameState (Cursor cursorTile) bs)
  | isValidTile (h bs) (w bs) newTile = GameState (Cursor newTile) bs
  | otherwise = GameState (Cursor cursorTile) bs
  where
    newTile = tupleSum moveTile cursorTile

toggleMode :: GameState -> GameState
toggleMode (GameState Coordinate bs) = GameState (Cursor (0, 0)) bs
toggleMode (GameState (Cursor _) bs) = GameState Coordinate bs

game :: GameState -> IO ()
game gs = do
  clearScreen
  print gs
  let res = getResult $ bs gs
  case res of
    Ongoing -> do
      cmd <- getCommand gs
      game $ doCommand cmd gs
    _ -> do gameOver res

getCommand :: GameState -> IO Command
getCommand (GameState Coordinate bs) = do
  putStrLn "Place or remove a Flag (F), Guess a safe square (G), or Toggle input mode (T)"
  c <- getSingleChar "Invalid input, supply a single character (F|G|T)" (`elem` "FGTfgt")
  case toUpper c of
    'F' -> do
      tile <- getTile (h bs) (w bs)
      return (Move Flag tile)
    'G' -> do
      tile <- getTile (h bs) (w bs)
      return (Move Guess tile)
    'T' -> do return ModeToggle
    _ -> error "Could not determine MoveType"
getCommand (GameState (Cursor tile) bs) = do
  putStrLn "Place or remove a Flag (F), Guess a safe square (G), move the cursor (WASD) or Toggle input mode (T)"
  c <- getSingleChar "Invalid input, supply a single character (F|G|W|A|S|D|T)" (`elem` "FGWASDTfgwasdt")
  case toUpper c of
    'F' -> do
      return (Move Flag tile)
    'G' -> do
      return (Move Guess tile)
    'T' -> do return ModeToggle
    'W' -> do return (CursorMove (-1, 0))
    'A' -> do return (CursorMove (0, -1))
    'S' -> do return (CursorMove (1, 0))
    'D' -> do return (CursorMove (0, 1))
    _ -> error "Could not determine MoveType"

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
  c <- getSingleChar "Invalid input, supply a single character (F|G)" (`elem` "FGfg")
  case toUpper c of
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

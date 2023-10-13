{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Evaluate" #-}

module Main where
import Data.List (group)
import System.Random (getStdGen, randomRIO)
import Text.Read (readMaybe)
import Control.Monad (replicateM)

type Cell = Char
type Board = [[Cell]]

matrixDimension :: IO (Maybe (Int, Int))
matrixDimension = do
  putStrLn "Enter the number of rows:"
  rowsStr <- getLine
  putStrLn "Enter the number of columns:"
  colsStr <- getLine
  let rows = readMaybe rowsStr :: Maybe Int
      cols = readMaybe colsStr :: Maybe Int
  return $ do
    r <- rows
    c <- cols
    return (r, c)

states :: [Char]
states = ['v', 'm', 'z']

createBoard :: Int -> Int -> IO Board
createBoard rows cols = do
  gen <- getStdGen
  let getRandomState = randomRIO (0, length states - 1)
  randomMatrix <- replicateM rows (replicateM cols getRandomState)
  return $ map (map (states !!)) randomMatrix

printBoard :: Board -> IO ()
printBoard = mapM_ (\row -> putStrLn (unwords [[c] | c <- row]))

checkAdjacentCells :: Board -> Int -> Int -> [(Cell, Int)]
checkAdjacentCells board i j = 
    let neighbors = [
          (x, y) | 
          x <- [i - 1..i + 1], 
          y <- [j - 1..j + 1], 
          x /= i || y /= j, 
          x >= 0, y >= 0, 
          x < length board, y < length (head board)
          ]
        adjacentStates = [board !! x !! y | (x, y) <- neighbors]
        countStates = [(state, length (filter (== state) adjacentStates)) | state <- "vmz"]
    in countStates

infecction :: [(Cell, Int)] -> Bool
infecction countStates = case lookup 'z' countStates of 
  Just qz -> qz >= 1
  Nothing -> False

subpopulation :: [(Cell, Int)] -> Bool
subpopulation countStates = 
  let qv = case lookup 'v' countStates of 
        Just q -> q
        Nothing -> 0
      qz = case lookup 'z' countStates of
        Just q -> q
        Nothing -> 0
  in qv < 2 && qz == 0

superpopulation :: [(Cell, Int)] -> Bool
superpopulation countStates = 
    let qv = case lookup 'v' countStates of
            Just q -> q
            Nothing -> 0
        qz = case lookup 'z' countStates of
            Just q -> q
            Nothing -> 0
    in qv > 3 && qz == 0

reproduction :: [(Cell, Int)] -> Bool
reproduction countStates = case lookup 'v' countStates of
    Just qv -> qv == 3
    Nothing -> False

inanition :: [(Cell, Int)] -> Bool
inanition countStates = case lookup 'v' countStates of
    Just qv -> qv == 0
    Nothing -> False

updateCell :: Cell -> [(Cell, Int)] -> Cell
updateCell currCell adjacentCells
  | currCell == 'v' =
    if any infecction [adjacentCells]
      then 'z'
      else if any subpopulation [adjacentCells] || any superpopulation [adjacentCells]
        then 'm'
        else currCell
  | currCell == 'm' =
    if any reproduction [adjacentCells]
      then 'v'
      else currCell
    | otherwise =
      if any inanition [adjacentCells]
        then 'm'
        else currCell

iterateWithBoard :: [[Char]] -> [[Char]]
iterateWithBoard board =
  [[updateCell (board !! i !! j) (checkAdjacentCells board i j) | j <- [0..cols - 1]] | i <- [0..rows - 1]]
    where 
      rows = length board
      cols = length (head board)

{-Função para contar a ocorrencia de um mesmo elemento na matriz-}
countValues :: Eq a => [[a]] -> a -> Int
countValues matrix value = length $ concat $ filter ((== value) . head) $ group $ concat matrix

main :: IO ()
main = do
  dimensions <- matrixDimension
  case dimensions of
    Just (rows, cols) -> do
      board <- createBoard rows cols
      putStrLn "\nInitial Board:\n"
      printBoard board

      putStrLn "\nEnter the number of iterations (n):"
      iterationsStr <- getLine
      let n = read iterationsStr

      let i = 1
      let iterate i board
            | i > n = return ()
            | otherwise = do
                let newBoard = iterateWithBoard board
                putStrLn $ "\n" ++ show i ++ "ª iteração:\n"
                printBoard newBoard
                iterate (i + 1) newBoard
      iterate i board
    Nothing -> putStrLn "Invalid dimensions entered."
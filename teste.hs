{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Evaluate" #-}

module Main where
import Data.List ( group, intercalate )
import Text.Read (readMaybe)
import Control.Monad (replicateM)
import System.IO

type Cell = Char
type Board = [[Cell]]

-- antes de iniciar a execução do arquivo teste.hs, insira dados no arquivo entradas.txt
main :: IO ()
main = do
  dimensions <- matrixDimension
  case dimensions of
    Just (rows, cols) -> do
      board <- createBoard rows cols "entradas.txt"
      putStrLn "\nMatriz inicial:\n"
      printBoard board

      putStrLn "\nInforme o número de interações (n):"
      iterationsStr <- getLine
      let n = read iterationsStr

      let i = 1
      let iterate i board
            | i > n = return ()
            | otherwise = do
                let newBoard = iterateWithBoard board
                if newBoard == board
                  then putStrLn $ "Estabilidade após " ++ show (i-1) ++ " iterações."
                  else do
                    putStrLn $ "\n" ++ show i ++ "ª iteração:\n"
                    printBoard newBoard
                    iterate (i + 1) newBoard
      iterate i board
    Nothing -> putStrLn "Dimensões inválidas."

matrixDimension :: IO (Maybe (Int, Int))
matrixDimension = do
  putStrLn "Informe o numero de linhas:"
  rowsStr <- getLine
  putStrLn "Informe o numero de colunas:"
  colsStr <- getLine
  let rows = readMaybe rowsStr :: Maybe Int
      cols = readMaybe colsStr :: Maybe Int
  return $ do
    r <- rows
    c <- cols
    return (r, c)



createBoard :: Int -> Int -> FilePath -> IO Board
createBoard rows cols filePath = do
  fileContent <- readFile filePath
  let fileLines = lines fileContent
      matrixData = take (rows * cols) fileLines
  return $ formMatrix rows cols (concat matrixData)

formMatrix :: Int -> Int -> [a] -> [[a]]
formMatrix i j lst = take i $ map (take j) $ iterate (drop j) lst



printBoard :: Board -> IO ()
printBoard = mapM_ (\row -> putStrLn (unwords [c : " " | c <- row]))



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
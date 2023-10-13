module Main where
import Data.List (group)
import System.Random (getStdGen, randomRIO)
import Text.Read (readMaybe)
import Control.Monad (replicateM)

matrixDimension :: IO (Maybe (Int, Int))
matrixDimension = do
  putStrLn "Enter the number of rows:"
  rowsStr <- getLine
  putStrLn "Enter the number of columns:"
  colsStr <- getLine
  putStrLn "\n"
  let rows = readMaybe rowsStr :: Maybe Int
      cols = readMaybe colsStr :: Maybe Int
  return $ do
    r <- rows
    c <- cols
    return (r, c)

states :: [Char]
states = ['v', 'm', 'z']

createBoard :: Int -> Int -> IO [[Char]]
createBoard rows cols = do
  gen <- getStdGen
  let getRandomState = randomRIO (0, length states - 1)
  randomMatrix <- replicateM rows (replicateM cols getRandomState)
  return $ map (map (states !!)) randomMatrix

checkAdjacentCells :: [[Char]] -> Int -> Int -> [(Char, Int)]
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

{-Função para contar a ocorrencia de um mesmo elemento na matriz-}
countValues :: Eq a => [[a]] -> a -> Int
countValues matrix value = length $ concat $ filter ((== value) . head) $ group $ concat matrix

main :: IO ()
main = do
  dimensions <- matrixDimension
  case dimensions of
    Just (rows, cols) -> do
      matrix <- createBoard rows cols
      mapM_ (\row -> putStrLn (unwords [[c] | c <- row])) matrix

      let i = 1
          j = 1
      let adjacentCells = checkAdjacentCells matrix i j 
      putStrLn "\nTeste da função checkAdvacentCells:\nContagem de células adjacentes para celula (1,1):"
      mapM_ (\(state, count) -> putStrLn $ state : " - " ++ show count) adjacentCells
    Nothing -> putStrLn "Invalid dimensions entered."

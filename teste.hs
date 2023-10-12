{-# LANGUAGE PackageImports #-}

module Main where
import  System.Random (getStdGen)


import Text.Read (readMaybe)

{-Função para pedir dimensões da matriz-}
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
      

{-Função para popular matriz-}
populateMatrix :: Int -> [a] -> [[a]]
populateMatrix n values = take n $ repeat values

main :: IO ()
main = do
  let states = ["morto","vivo","zumbi"] {-Lista de estados possiveis-}
  matrixDimension

{-Pupulando a matriz com valores randomizados da lista-}
  gen <- getStdGen
  let values = take (rows * cols) $ map (states !!) $ randomRs (0, length states - 1) gen
      matrix = populateMatrix rows (chunksOf cols values)
  print matrix
  where
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)
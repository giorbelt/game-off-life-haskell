{-# LANGUAGE PackageImports #-}

module Main where
import System.Random (getStdGen, randomRs)
import Text.Read (readMaybe)

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

populateMatrix :: Int -> [a] -> [[a]]
populateMatrix n values = take n $ repeat values

main :: IO ()
main = do
  let states = ["morto", "vivo", "zumbi"]  -- Lista de estados possíveis
  dimensions <- matrixDimension
  case dimensions of
    Just (rows, cols) -> do
      gen <- getStdGen
      let values = take (rows * cols) $ map (states !!) $ randomRs (0, length states - 1) gen
          matrix = populateMatrix rows (chunksOf cols values)
      print matrix
    Nothing -> putStrLn "Invalid dimensions entered."
  where
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

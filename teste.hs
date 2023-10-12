{-# LANGUAGE PackageImports #-}

module Main where
import System.Random (getStdGen, randomRIO)
import Text.Read (readMaybe)
import Control.Monad (replicateM)

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

states :: [String]
states = ["morto", "zumbi", "vivo"]

populateMatrix :: Int -> Int -> IO [[String]]
populateMatrix rows cols = do
    gen <- getStdGen
    let getRandomState = randomRIO (0, length states - 1)
    randomMatrix <- replicateM rows (replicateM cols getRandomState)
    return $ map (map (states !!)) randomMatrix

main :: IO ()
main = do
  dimensions <- matrixDimension
  case dimensions of
    Just (rows, cols) -> do
      matrix <- populateMatrix rows cols
      mapM_ print matrix
    Nothing -> putStrLn "Invalid dimensions entered."

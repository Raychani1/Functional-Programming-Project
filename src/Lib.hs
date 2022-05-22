{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( project,
  )
where

import Data.String
import Data.Typeable
import Data.Function
import Parser
import Data.Map
import Data.List (iterate', head, map)
import System.Directory
import System.Environment
import System.FilePath
import System.IO(IOMode(WriteMode), openFile, hClose, hPrint)

-- | Returns List of Search Queries
getSearchQueries :: IO [String]
getSearchQueries = Prelude.concatMap Data.String.words <$> getArgs

-- | Returns the first [numberOfLines] lines of Input Data
readData :: FilePath -> Int -> IO [String]
readData filePath numberOfLines = do
  -- Read Contents of File
  content :: String <- Prelude.readFile filePath

  -- Split Content of File to Lines
  let linesOfFiles :: [String] = Data.String.lines content

  -- Return the first [numberOfLines] Lines of Data
  return (Prelude.take numberOfLines linesOfFiles)

project :: IO ()
project = do
  -- | Remove duplicates from [String]
  -- removeDuplicates :: [String] -> [String]
  -- removeDuplicates [] = []
  -- removeDuplicates (x:xs) = x : (removeDuplicates (remove x xs))
  --   where 
  --     remove :: String -> [String] -> [String]
  --     remove x [] = []
  --     remove x (y:ys) 
  --       | x == y = remove x ys
  --       | otherwise = y:(remove x ys)
  -- myFix :: (Ord a, Floating a) => a -> a
  -- myFix x = converge (\x y -> abs(x-y) < 0.0001) $ iterate' f x 
  --   where f = cos

  -- print myFix 400.0
  
  -- -- Fetch Search Queries from CLI
  searchQueries :: [String] <- getSearchQueries

  -- print searchQueries

  -- -- Get Current Work Directory
  cwd :: FilePath <- getCurrentDirectory


  -- -- Read Input Data File
  inputData :: [String] <- readData (joinPath [cwd, "data", "test_data.json"]) 30

  
  let processData = processAllInputs inputData
  let forPageRank = processForPageRank processData
  let result = iterate calculatePageRanks forPageRank !! 50
  -- if you want delete some key:value pair, you can do so by in Lib.hs in class calculateNewPageRank


  -- Write results to file
  -- let resultsLedger = [(i, results !! i ) | i <- [0 .. 10]]
  outh <- openFile (joinPath [cwd, "output.txt"]) WriteMode
  hPrint outh result
  hClose outh

  return ()
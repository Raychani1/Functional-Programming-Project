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
  inputData :: [String] <- readData (joinPath [cwd, "data", "data.json"]) 100

  
  let results = processAllInputs inputData

  --print results

  let forPageRank = processForPageRank results


  let a = iterate calculatePageRanks forPageRank !! 40

  print (Data.List.map (\x -> Data.List.head (x ! "page_rank")) a)




  

  -- let pageRanks = calculatePageRanks forPageRank
  -- print (Data.List.map (\x -> Data.List.head (x ! "page_rank")) pageRanks)
  -- let pageRanks2 = calculatePageRanks pageRanks
  -- print (Data.List.map (\x -> Data.List.head (x ! "page_rank")) pageRanks2)
  -- pageRanks = calculatePageRanks pageRanks
  -- print (Data.Map.map (\x -> Data.List.head (x ! "page_rank")) pageRanks)
  -- pageRanks = calculatePageRanks pageRanks
  -- print (Data.Map.map (\x -> Data.List.head (x ! "page_rank")) pageRanks)
  -- pageRanks = calculatePageRanks pageRanks
  -- print (Data.Map.map (\x -> Data.List.head (x ! "page_rank")) pageRanks)
  -- pageRanks = calculatePageRanks pageRanks
  -- print (Data.Map.map (\x -> Data.List.head (x ! "page_rank")) pageRanks)

  --print $ results
  -- print $ forPageRank

  -- let url = ( head(results) ! "url")

  --print (url `elem` ["https://www.one.com", "valami mas"])
  --print (url `elem` ["https://wwws.one.com", "valami mas"])

  -- let results = inputData
  -- Write results to file
  -- let resultsLedger = [(i, results !! i ) | i <- [0 .. 10]]

  -- outh <- openFile (joinPath [cwd, "output.txt"]) WriteMode
  -- hPrint outh results
  -- hClose outh

  -- print  $ head results
  -- print (show(typeOf results))


  -- print $ toList $ head results

  -- --fromListToList :: [Map String [String]] -> [Map String [String]]

  -- print $ show(typeOf (toList $ head results))

  return ()
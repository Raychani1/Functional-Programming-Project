{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( project,
  )
where

import Data.String
import Parser
import System.Directory
import System.Environment
import System.FilePath
import System.IO

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
  -- Fetch Search Queries from CLI
  searchQueries :: [String] <- getSearchQueries

  -- Get Current Work Directory
  cwd :: FilePath <- getCurrentDirectory

  -- Read Input Data File
  inputData :: [String] <- readData (joinPath [cwd, "data", "data.json"]) 3000

  let results = processAllInputs inputData

  -- let resultsLedger = [(i, results !! i) | i <- [168]]
  let resultsLedger = [(i, results !! i) | i <- [168]]

  outh <- openFile (joinPath [cwd, "output", "output168.txt"]) WriteMode
  hPrint outh resultsLedger
  hClose outh

  outh <- openFile (joinPath [cwd, "output", "input_168.txt"]) WriteMode
  hPrint outh (inputData !! 168)
  hClose outh

  return ()
module Lib
  ( project,
  )
where

import Data.String
import System.Directory
import System.Environment
import System.FilePath

-- | Returns List of Search Queries
getSearchQueries :: IO [String]
getSearchQueries = do concatMap words <$> getArgs

-- | Returns the first [numberOfLines] lines of Input Data
readData :: FilePath -> Int -> IO [String]
readData filePath numberOfLines = do
  -- Read Contents of File
  content <- readFile filePath

  -- Split Content of File to Lines
  let linesOfFiles = Data.String.lines content

  -- Return the first [numberOfLines] Lines of Data
  return (Prelude.take numberOfLines linesOfFiles)

project :: IO ()
project = do
  -- Fetch Search Queries from CLI
  searchQueries <- getSearchQueries

  print searchQueries

  -- Get Current Work Directory
  cwd <- getCurrentDirectory

  -- Read Input Data File
  inputData <- readData (joinPath [cwd, "data", "data.json"]) 200

  print inputData

  return ()
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

project :: IO ()
project = do
  -- Fetch Search Queries from CLI
  searchQueries <- getSearchQueries

  print searchQueries

  return ()
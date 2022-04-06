{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( project,
  )
where

import Data.Aeson
import Data.ByteString.Lazy.Char8
import Data.Map
import Data.Maybe
import Data.String
import GHC.Generics
import System.Directory
import System.Environment
import System.FilePath

-- | Returns List of Search Queries
getSearchQueries :: IO [String]
getSearchQueries = do Prelude.concatMap Data.String.words <$> getArgs

-- | Returns the first [numberOfLines] lines of Input Data
readData :: FilePath -> Int -> IO [String]
readData filePath numberOfLines = do
  -- Read Contents of File
  content :: String <- Prelude.readFile filePath

  -- Split Content of File to Lines
  let linesOfFiles :: [String] = Data.String.lines content

  -- Return the first [numberOfLines] Lines of Data
  return (Prelude.take numberOfLines linesOfFiles)

-- | Converts Maybe Map to Just Map
maybeMapToMap :: Maybe (Map k v) -> Map k v
maybeMapToMap (Just myMap) = myMap
maybeMapToMap Nothing = Data.Map.empty

project :: IO ()
project = do
  -- Fetch Search Queries from CLI
  searchQueries :: [String] <- getSearchQueries

  print searchQueries

  -- Get Current Work Directory
  cwd :: FilePath <- getCurrentDirectory

  -- Read Input Data File
  inputData :: [String] <- readData (joinPath [cwd, "data", "data.json"]) 200

  let dataMap :: Map String String = maybeMapToMap (Data.Aeson.decode (pack (Prelude.head inputData)) :: Maybe (Map String String))

  print (dataMap ! "url")

  return ()
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( project,
  )
where

import Data.Aeson
import Data.ByteString.Lazy.Char8
import Data.Map
import Data.String
import System.Directory
import System.Environment
import System.FilePath
import Text.HTML.Scalpel
import Text.Regex.TDFA

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

-- | Converts Maybe String to Just String
maybeStringToString :: Maybe String -> String
maybeStringToString (Just myString) = myString
maybeStringToString Nothing = ""

project :: IO ()
project = do
  -- Fetch Search Queries from CLI
  searchQueries :: [String] <- getSearchQueries

  -- Get Current Work Directory
  cwd :: FilePath <- getCurrentDirectory

  -- Read Input Data File
  inputData :: [String] <- readData (joinPath [cwd, "data", "data.json"]) 200

  let dataMap :: Map String String = maybeMapToMap (Data.Aeson.decode (pack (Prelude.head inputData)) :: Maybe (Map String String))

  -- let body = (dataMap ! "html_content") =~ "<body[^>]*>([\\w\\W]*)<\\/body>" :: String -- Regex not matching

  let body = maybeStringToString (scrapeStringLike (dataMap ! "html_content") (innerHTML (tagSelector "body")))

  print body

  return ()
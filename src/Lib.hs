{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( project,
  )
where

import Data.String
import Data.Typeable
import Data.Function
import Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Lazy as DBL
import Parser
import System.IO
import Data.Map
import Data.Aeson
import Data.List (iterate', head, map)
import System.Directory
import System.FilePath
import qualified Data.Map as M
import Data.Typeable
import Converters

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
  -- Get Current Work Directory
  cwd :: FilePath <- getCurrentDirectory

  -- -- Read Input Data File
  inputData :: [String] <- readData (joinPath [cwd, "data", "test_data.json"]) 30

  let processData = processAllInputs inputData
  let forPageRank = processForPageRank processData
  let result = Prelude.iterate calculatePageRanks forPageRank !! 50
  -- if you want delete some key:value pair, you can do so by in Lib.hs in class calculateNewPageRank

  -- reverse index
  let resultReverseIndexed = rvProcessReverseIndex result

  -- save reverse index result as json
  let resultReverseIndexedEncodedJson = Data.Aeson.encode $ toJSON resultReverseIndexed
  DBL.writeFile "data/output.json" resultReverseIndexedEncodedJson

  return ()
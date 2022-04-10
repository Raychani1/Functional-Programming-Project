{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( project,
  )
where

import Data.Aeson
import Data.ByteString.Lazy.Char8
import Data.List
import Data.Map
import Data.String
import Hakyll.Web.Html
import System.Directory
import System.Environment
import System.FilePath
import Text.HTML.Scalpel
import Text.Regex (mkRegex, subRegex)
import Text.XML.HXT.DOM.Util

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

-- | Processes HTML Body
processBody :: String -> [[String]]
processBody body = do
  let quotesAdded = subRegex (mkRegex "&quot;") body "\""

  let apostropheAdded = subRegex (mkRegex "&#39;") quotesAdded "'"

  let ampersandAdded = subRegex (mkRegex "&amp;") apostropheAdded "&"

  let tagStartsAdded = subRegex (mkRegex "&lt;") ampersandAdded "<"

  let tagEndsAdded = subRegex (mkRegex "&gt;") tagStartsAdded ">"

  let removedBraces = subRegex (mkRegex "<[^>]*>|\\{[^\\}]*\\}|\\([^\\)]*\\)") tagEndsAdded " "

  let removedTags = stripTags removedBraces

  let breakLinesRemoved = subRegex (mkRegex "\\\n") removedTags " "

  let specialCharactersOverwritten1 = subRegex (mkRegex "\\\169|\\\186") breakLinesRemoved " "

  let specialCharactersOverwritten2 = subRegex (mkRegex "\\\193|\\\224|\\\225|\\\226|\\\227") specialCharactersOverwritten1 "a"

  let specialCharactersOverwritten3 = subRegex (mkRegex "\\\231") specialCharactersOverwritten2 "c"

  let specialCharactersOverwritten4 = subRegex (mkRegex "\\\201|\\\233|\\\234") specialCharactersOverwritten3 "e"

  let specialCharactersOverwritten5 = subRegex (mkRegex "\\\205|\\\237") specialCharactersOverwritten4 "i"

  let specialCharactersOverwritten6 = subRegex (mkRegex "\\\211|\\\243|\\\244|\\\245") specialCharactersOverwritten5 "o"

  let specialCharactersOverwritten7 = subRegex (mkRegex "\\\218|\\\250") specialCharactersOverwritten6 "u"

  let specialCharactersRemoved = subRegex (mkRegex "[^0-9a-zA-Z]+") specialCharactersOverwritten7 " "

  let uniqueWords = sort (Data.List.nub (Prelude.words (stringToLower specialCharactersRemoved)))

  return uniqueWords

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

  let body = maybeStringToString (scrapeStringLike (dataMap ! "html_content") (innerHTML (tagSelector "body")))

  let processedBody = Prelude.concat (processBody body)

  print processedBody

  return ()
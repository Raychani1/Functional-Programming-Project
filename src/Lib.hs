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
import qualified Data.Text as T
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize
import Hakyll.Web.Html
import System.Directory
import System.Environment
import System.FilePath
import Text.HTML.Scalpel
import Text.Regex (mkRegex, subRegex)
import Text.XML.HXT.DOM.Util

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

-- | Removes Accent from String
-- | SOURCE: https://stackoverflow.com/a/44290219/14319439
canonicalForm :: String -> String
canonicalForm s = T.unpack noAccents
  where
    noAccents = T.filter (not . property Diacritic) normalizedText
    normalizedText = normalize NFD (T.pack s)

-- | Processes HTML Body
processBody :: String -> [String]
processBody body = uniqueWords
  where
    -- Replace HTML Quote Escape Character
    quotesAdded :: String = subRegex (mkRegex "&quot;") body "\""

    -- Replace HTML Apostrophe Escape Character
    apostropheAdded :: String = subRegex (mkRegex "&#39;") quotesAdded "'"

    -- Replace HTML Ampersand Escape Character
    ampersandAdded :: String = subRegex (mkRegex "&amp;") apostropheAdded "&"

    -- Replace HTML Greater Than Escape Character
    tagStartsAdded :: String = subRegex (mkRegex "&lt;") ampersandAdded "<"

    -- Replace HTML Less Than Escape Character
    tagEndsAdded :: String = subRegex (mkRegex "&gt;") tagStartsAdded ">"

    -- Remove braces and their content
    removedBraces :: String = subRegex (mkRegex "<[^>]*>|\\{[^\\}]*\\}|\\([^\\)]*\\)") tagEndsAdded " "

    -- Remove remaining tags from HTML
    removedTags :: String = stripTags removedBraces

    -- Remove break lines
    breakLinesRemoved :: String = subRegex (mkRegex "\\\n") removedTags " "

    -- Remove accent from HTML
    accentRemoved :: String = canonicalForm breakLinesRemoved

    -- Remove Special Characters
    specialCharactersOverwritten :: String = subRegex (mkRegex "\\\169|\\\186") accentRemoved " "

    specialCharactersRemoved :: String = subRegex (mkRegex "[^0-9a-zA-Z]+") specialCharactersOverwritten " "

    -- Get every unique word in HTML sorted lexicographically
    uniqueWords :: [String] = sort (Data.List.nub (Prelude.words (stringToLower specialCharactersRemoved)))

processInput :: [String] -> Map String [String]
processInput inputData = result
  where
    dataMap :: Map String String = maybeMapToMap (Data.Aeson.decode (pack (Prelude.head inputData)) :: Maybe (Map String String))

    body :: String = maybeStringToString (scrapeStringLike (dataMap ! "html_content") (innerHTML (tagSelector "body")))

    processedBody :: [String] = processBody body

    url :: [String] = Data.String.lines (dataMap ! "url")

    result :: Map String [String] = Data.Map.fromList [("url", url), ("words", processedBody)]

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

  let result = processInput inputData

  print (result ! "words")

  return ()
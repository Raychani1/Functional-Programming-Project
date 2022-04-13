{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import Converters
import Data.Aeson
import Data.ByteString.Lazy.Char8
import Data.List
import Data.Map
import Data.String
import qualified Data.Text as T
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize
import Hakyll.Web.Html
import Text.HTML.Scalpel
import Text.Regex (mkRegex, subRegex)
import Text.XML.HXT.DOM.Util

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
    specialCharactersOverwritten :: String = subRegex (mkRegex "\\\147|\\\148|\\\149\\\163|\\\167|\\\169|\\\170|\\\173|\\\176|\\\186|\\\187|\\\191|\\\215") accentRemoved " "

    specialCharactersRemoved :: String = subRegex (mkRegex "[^0-9a-zA-Z]+") specialCharactersOverwritten " "

    -- Get every unique word in HTML sorted lexicographically
    uniqueWords :: [String] = sort (Data.List.nub (Prelude.words (stringToLower specialCharactersRemoved)))

-- | Processes one line of Input
processInput :: Map String String -> Map String [String]
processInput dataMap = result
  where
    body :: String = maybeStringToString (scrapeStringLike (dataMap ! "html_content") (innerHTML (tagSelector "body")))

    processedBody :: [String] = processBody body

    url :: [String] = Data.String.lines (dataMap ! "url")

    result :: Map String [String] = Data.Map.fromList [("url", url), ("body", [body]), ("words", processedBody)]

-- | Processes every line of Input
processAllInputs :: [String] -> [Map String [String]]
processAllInputs inputData = results
  where
    allData :: [Map String String] = [maybeMapToMap (Data.Aeson.decode (pack line) :: Maybe (Map String String)) | line <- inputData]

    results :: [Map String [String]] = [processInput dataMap | dataMap <- allData]
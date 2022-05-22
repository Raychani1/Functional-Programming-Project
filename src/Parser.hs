{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import Converters
import Data.Aeson
import Data.ByteString.Lazy.Char8
import Data.List
import Data.Map
import Data.String
import Data.Foldable
import qualified Data.Text as T
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize
import Hakyll.Web.Html
import Text.HTML.Scalpel
import Text.Regex (mkRegex, subRegex)
import Text.XML.HXT.DOM.Util
import GHC.List
import Data.List (iterate')

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
    quotesAdded :: String = subRegex (mkRegex "&quot;") (Data.String.fromString body) "\""

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
    breakLinesRemoved :: String = subRegex (mkRegex "\\\n|\\\\") removedTags " "

    -- Remove accent from HTML
    accentRemoved :: String = canonicalForm breakLinesRemoved

    -- TODO - Find better solution
    -- Remove Special Characters
    specialCharactersOverwritten :: String = subRegex (mkRegex "\\\141|\\\145|\\\147|\\\148|\\\149|\\\163|\\\167|\\\169|\\\170|\\\171|\\\173|\\\174|\\\176|\\\178|\\\179|\\\185|\\\186|\\\187|\\\189|\\\191") accentRemoved ""

    specialCharactersRemoved :: String = subRegex (mkRegex "[^0-9a-zA-Z]+") specialCharactersOverwritten " "

    -- Get every unique word in HTML sorted lexicographically
    uniqueWords :: [String] = sort (Data.List.nub (Prelude.words (stringToLower specialCharactersRemoved)))

-- | Remove duplicates from list
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = Data.List.map Data.List.head . Data.List.group . sort

-- | Processes Hyperlink
processHyperlink :: String -> [String]
processHyperlink body = hyperlinks
  where
    hyperlinks_dup :: [String] = maybeStringListToStringList (scrapeStringLike body (attrs "href" (tagSelector("a")) ) )
    hyperlinks :: [String] = removeDuplicates hyperlinks_dup


-- | Processes one line of Input
processInput :: Map String String -> Map String [String] 
processInput dataMap = result
  where
    -- Extract body from HTML
    body :: String = maybeStringToString (scrapeStringLike (dataMap ! "html_content") (innerHTML (tagSelector "body")))
    
    -- Extract hyperlinks from body
    hyperlinks :: [String] = (processHyperlink body)
    
    -- Process extracted body
    processedBody :: [String] = processBody body

    -- Convert URL to List
    url :: [String] = Data.String.lines (dataMap ! "url")

    -- Create new Map of Processed Data
    result :: Map String [String] = Data.Map.fromList [("url", url), ("words", processedBody), ("links_out", hyperlinks)]

    -- create Map for PageRank computation

-- | Processes every line of Input
processAllInputs :: [String] -> [Map String [String]]
processAllInputs inputData = results
  where
    -- Read Data to Map
    allData :: [Map String String] = [maybeMapToMap (Data.Aeson.decode (pack line) :: Maybe (Map String String)) | line <- inputData]
    -- Process Data
    results :: [Map String [String]]  = [processInput dataMap | dataMap <- allData] 
    

incomingLinks :: [Map String [String]] -> [Map String [String]]
incomingLinks dataMap = result
  where
    urls :: [String] = Data.List.concat $ Data.List.map (\x -> (x Data.Map.! "url")) dataMap 
    result :: [Map String [String]] = Data.List.map (\url -> processUrl url dataMap) urls

processUrl :: String -> [Map String [String]] -> Map String [String]
processUrl url dataMap = result
  where
    filterDataMap :: [Map String [String]]= Data.List.filter (\x -> url `Data.List.elem` ( x Data.Map.! "links_out" )) dataMap
    urlsIn :: [String] = Data.List.map (\x -> Data.List.head (x Data.Map.! "url")) filterDataMap
    urlsOut ::[String] = getLinksOut url dataMap
    words :: [String] = getWords url dataMap
    result :: Map String [String] = Data.Map.fromList [("url", [url]), ("links_in", urlsIn), ("links_out", urlsOut), ("page_rank", ["0"]), ("words", words)]

processForPageRank :: [Map String [String]] ->  [Map String [String]]
processForPageRank inputData = results
  where
    results :: [Map String [String]] = incomingLinks inputData 

calculatePageRanks :: [Map String [String]] -> [Map String [String]] 
calculatePageRanks dataMap = calculate
  where
    calculate :: [Map String [String]] = Data.List.map (\x -> calculateNewPageRank x dataMap) dataMap
    
calculateNewPageRank :: Map String [String] -> [Map String [String]] -> Map String [String]
calculateNewPageRank mapEntry dataMap = result
  where
    d = 0.85
    url = Data.List.head $ mapEntry ! "url"
    words = getWords url dataMap
    links_in = mapEntry ! "links_in"
    page_rank = Data.List.head $ mapEntry ! "page_rank"
    linksInNumbers :: [Double] = Data.List.map (\x -> getPageRank x dataMap / fromIntegral (Data.List.length (getLinksOut x dataMap)) ) links_in
    newPageRank = (1 - d) + d * (Data.Foldable.sum linksInNumbers)
    result = Data.Map.fromList [("url", [url]), ("links_in", links_in), ("links_out", (mapEntry ! "links_out")), ("page_rank", [show newPageRank]), ("words", words)]

getLinksOut :: String -> [Map String [String]] -> [String]
getLinksOut url dataMap = Data.List.head (Data.List.filter (\x -> url `Data.List.elem` ( x Data.Map.! "url" )) dataMap ) Data.Map.! "links_out"

getLinksIn :: String -> [Map String [String]] -> [String]
getLinksIn url dataMap = Data.List.head (Data.List.filter (\x -> url `Data.List.elem` ( x Data.Map.! "url" )) dataMap ) Data.Map.! "links_in"

getPageRank :: String -> [Map String [String]] -> Double
getPageRank url dataMap = read $ Data.List.head $ Data.List.head (Data.List.filter (\x -> url `Data.List.elem` ( x Data.Map.! "url" )) dataMap ) Data.Map.! "page_rank"

getWords :: String -> [Map String [String]] -> [String]
getWords url dataMap = Data.List.head (Data.List.filter (\x -> url `Data.List.elem` ( x Data.Map.! "url" )) dataMap ) Data.Map.! "words"

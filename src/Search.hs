{-# LANGUAGE ScopedTypeVariables #-}

module Search where

import Data.String
import Data.Ord (comparing)
import Data.List (intersect, sortBy)
import System.Environment
import qualified Data.ByteString.Lazy as DBL
import Converters
import Data.Aeson
import Control.DeepSeq (deepseq)

-- | Returns List of Search Queries
getSearchQueries :: IO [String]
getSearchQueries = Prelude.concatMap Data.String.words <$> getArgs

getInterSectUrls :: [[(String, Double)]] -> [(String, Double)]
getInterSectUrls [] = []
getInterSectUrls resultUrls = foldl1 intersect resultUrls

search :: IO ()
search = do

  -- Fetch Search Queries from CLI
  searchQueries :: [String] <- getSearchQueries

  -- read rv json
  rvJsonFile <- DBL.readFile "data/output.json"
  let database :: [(String, [(String, Double)])] = maybeNightmareObjectToObject $ Data.Aeson.decode rvJsonFile
  let dummy = database `deepseq` 42

  let resultUrls :: [[(String, Double)]] = map (\x -> snd x) $filter (\entry -> (fst entry) `elem` searchQueries) database
  let intersectUrls :: [(String, Double)] = getInterSectUrls resultUrls
  let sortedUrls :: [(String, Double)] = sortBy (flip $ comparing snd) intersectUrls

  print $ take 10 sortedUrls

  return ()

main :: IO ()
main = search

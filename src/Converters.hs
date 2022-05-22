module Converters where

import Data.Map

-- | Converts Maybe Map to Just Map
maybeMapToMap :: Maybe (Map k v) -> Map k v
maybeMapToMap (Just myMap) = myMap
maybeMapToMap Nothing = Data.Map.empty

-- | Converts Maybe String to Just String
maybeStringToString :: Maybe String -> String
maybeStringToString (Just myString) = myString
maybeStringToString Nothing = ""

-- | Converts Maybe [String] to Just [String]
maybeStringListToStringList :: Maybe [String] -> [String]
maybeStringListToStringList (Just myString) = myString
maybeStringListToStringList Nothing = [""]
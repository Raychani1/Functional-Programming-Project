module Lib
    ( project
    ) where

import System.Environment

splitSearchQuery searchQuery =  do
    return ( concatMap words searchQuery )


project :: IO ()
project = do
    userInput <- getArgs

    splitUserInput <- splitSearchQuery userInput

    print splitUserInput


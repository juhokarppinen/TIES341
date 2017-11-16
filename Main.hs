{-  Text-to-HTML
    A simple Haskell project for TIES341 Functional Programming 2

    By Juho Karppinen 2017


    This program reads a data file and outputs the contents formatted as HTML 
    tables. -}

module Main where

import Control.Monad
import Data.Monoid
import System.IO
import System.Environment
import System.FilePath
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

l = readDataToList inputFile

inputFile :: String
inputFile = "data/gig-data.txt"

outputFile :: String
outputFile = "html/gigs.html"


main :: IO ()
main = do
    putStrLn "Hello world"


-- Read Text data from a speficied filepath and return it as a list of lines.
readDataToList :: String -> IO [Text.Text]
readDataToList file = do
    dataLines <- fmap Text.lines (Text.readFile file)
    return dataLines


-- Append a list of lines into a file.
appendToFile :: String -> [Text.Text] -> IO ()
appendToFile file lines = do
    let lines2 = fmap (flip Text.snoc $ '\n') lines
    mapM_ (Text.appendFile file) lines2


-- Pretty print a list of IO lines.
printLines :: IO [Text.Text] -> IO ()
printLines t = do
    textLines <- t
    mapM_ Text.putStrLn textLines


-- Perform a functor operation to a functor inside a monad.
-- For example:
--
--      fmapM reverse $ readDataToList inputFile
fmapM :: (Functor f, Monad m) => (f a -> f b) -> m (f a) -> m (f b)
fmapM op a = a >>= (\b -> return $ op b)
    
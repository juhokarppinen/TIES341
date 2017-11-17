{-  
    +---------------------------------------------------------------+
    | Text-to-HTML                                                  |
    | A simple Haskell project for TIES341 Functional Programming 2 |
    |                                                               |
    | By Juho Karppinen 2017                                        |
    +---------------------------------------------------------------+

    This program reads a data file and outputs the contents formatted as HTML 
    tables. 

    Non-base dependencies:

        Data.Time.Split (install split)
-}

module Main where

import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Data.Text
import Data.Time
import Data.Monoid
import System.IO
import System.Environment
import System.FilePath
import qualified Data.Text    as T
import qualified Data.Text.IO as T

{- NOTES

fromGregorian :: Integer -> Int -> Int -> Day

    Convert from proleptic Gregorian calendar. First argument is year, 
    second month number (1-12), third day (1-31). Invalid values will be 
    clipped to the correct range, month first, then day.
-}
data Entry = Entry { 
    getDate :: Day,
    getName :: T.Text,
    getPlace :: T.Text
    } deriving Show


inputFile :: String
inputFile = "data/gig-data.txt"

outputFile :: String
outputFile = "html/gigs.html"


main :: IO ()
main = do
    putStrLn "Hello world"


-- Read Text data from a speficied filepath and return it as a list of lines.
readDataToList :: String -> IO [T.Text]
readDataToList file = do
    dataLines <- fmap T.lines (T.readFile file)
    return dataLines


-- Append a list of lines into a file.
appendToFile :: String -> [T.Text] -> IO ()
appendToFile file lines = do
    let lines2 = fmap (flip T.snoc $ '\n') lines
    mapM_ (T.appendFile file) lines2


-- Pretty print a list of IO lines.
printLines :: IO [T.Text] -> IO ()
printLines t = do
    textLines <- t
    mapM_ T.putStrLn textLines


-- Return a Day into a Finnish formatted text representation.
showDateFinnishFormat :: Day -> T.Text
showDateFinnishFormat d = T.pack $ 
                          Data.List.intercalate "." $ 
                          Data.List.reverse $ 
                          Data.List.Split.splitOn "-" $ 
                          show d


-- Parse a Finnish formatted text representation of a date into a Day.
-- Brittle first implementation.
parseDay :: T.Text -> Maybe Day
parseDay t = case (Data.List.Split.splitOn "." $ unpack t) of
    d:(m:(y:[])) -> Just $ fromGregorian
        ((read y)::Integer)
        ((read m)::Int)
        ((read d)::Int) 
    otherwise    -> Nothing


-- Perform a functor operation to a functor inside a monad.
-- Useful for various IO [Text] objects I'm bound to come across.
--
-- For example:
--
--      fmapM reverse $ readDataToList inputFile
fmapM :: (Functor f, Monad m) => (f a -> f b) -> m (f a) -> m (f b)
fmapM op a = a >>= (\b -> return $ op b)
    
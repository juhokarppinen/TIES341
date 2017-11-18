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

        Data.List.Split (install split)
-}

module Main where

import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Data.Time
import Data.Monoid
import System.IO
import System.Environment
import System.FilePath
import Text.ParserCombinators.ReadP

{- NOTES

fromGregorian :: Integer -> Int -> Int -> Day

    Convert from proleptic Gregorian calendar. First argument is year, 
    second month number (1-12), third day (1-31). Invalid values will be 
    clipped to the correct range, month first, then day.
-}
data Entry = Entry { 
    getDate :: Maybe Day,
    getName :: Maybe String,
    getPlace :: Maybe String
    } deriving Show


-- Initial hardcoded filepath.
configFile :: String
configFile = "conf/config.cnf"


-- Initial hardcoded filepath.
inputFile :: String
inputFile = "data/gig-data.txt"


-- Initial hardcoded filepath.
outputFile :: String
outputFile = "html/gigs.html"


-- Testing shortcuts --
h = readToList configFile
d = readToList inputFile
-----------------------

main :: IO ()
main = do
    entryData <- readToList inputFile
    let entries = map parse entryData
    (putStrLn . show) entries


-- Parse a String into an Entry.
parse :: String -> Entry
parse s = getEntryFromParse $ readP_to_S parseEntry s


-- Unpack the data structure from a parsing result.
getEntryFromParse :: [(Entry,String)] -> Entry
getEntryFromParse p = case p of
    []        -> Entry Nothing Nothing Nothing
    otherwise -> fst $ p !! 0


-- The parser combinator for the Entry data type.
parseEntry :: ReadP Entry
parseEntry = do
    day <- many1 $ satisfy (/= ' ')
    satisfy (== ' ')
    name <- many1 $ satisfy (/= '\8211')
    string "\8211 "
    place <- many1 $ satisfy isAscii
    eof 
    return $ Entry (parseDay day) (Just name) (Just place)


-- Return a file's contents as a list of lines.
readToList :: String -> IO [String]
readToList file = do
    ls <- fmap lines (readFile file)
    return ls


-- Append a list of lines into a file.
appendToFile :: String -> [String] -> IO ()
appendToFile file lines = do
    let lines2 = fmap (++ "\n") lines
    mapM_ (appendFile file) lines2


-- Pretty print a list of showable IO objects.
printLines :: Show a => IO [a] -> IO ()
printLines t = do
    textLines <- t
    mapM_ (putStrLn . show) textLines


-- Generate an HTML table as a list of lines. 
generateTable :: IO [String] -> IO [String]
generateTable texts = do
    t0 <- fmapM (fmap (enclose "th")) $ readToList configFile
    let headers = encloseL "tr" t0
    -- TODO : Add data here
    return $ encloseL "table" headers
    

-- Enclose a String in HTML tags.
enclose :: String -> String -> String
enclose tag txt = "<" ++ tag ++ ">" ++ txt ++ "</" ++ tag ++ ">"


-- Enclose a [String] in HTML tags.
encloseL :: String -> [String] -> [String]
encloseL tag txts = ["<" ++ tag ++ ">"] ++ txts ++ ["</" ++ tag ++ ">"]


-- Return a Day into a Finnish formatted text representation.
showDateFinnishFormat :: Day -> String
showDateFinnishFormat d = Data.List.intercalate "." $ 
                          Data.List.reverse $ 
                          Data.List.Split.splitOn "-" $ 
                          show d


-- Parse a Finnish formatted text representation of a date into a Day.
-- Brittle first implementation.
parseDay :: String -> Maybe Day
parseDay t = case (Data.List.Split.splitOn "." $ t) of
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
    
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

{- 
NOTES

fromGregorian :: Integer -> Int -> Int -> Day

    Convert from proleptic Gregorian calendar. First argument is year, 
    second month number (1-12), third day (1-31). Invalid values will be 
    clipped to the correct range, month first, then day.
-}
data Entry = Entry { 
    getDate :: Day,
    getName :: String,
    getPlace :: String
    } deriving Show


instance Monoid Day where
    mempty = fromGregorian 0 0 0
    mappend a b = max a b


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
    entryData <- readToList inputFile -- Read input data
    let entries = map parse entryData -- Parse input data
    ls <- generateTable entries
    mapM_ putStrLn ls -- Print output HTML
    -- mapM_ (putStrLn.show) entries -- Print entries


-- Parse a String into an Entry.
parse :: String -> Entry
parse s = getEntryFromParse $ readP_to_S parseEntry s


-- Unpack the data structure from a parsing result.
getEntryFromParse :: [(Entry,String)] -> Entry
getEntryFromParse p = case p of
    []        -> Entry mempty mempty mempty
    otherwise -> fst $ p !! 0


-- The parser combinator for the Entry data type.
parseEntry :: ReadP Entry
parseEntry = do
    day <- many1 $ satisfy (/= ' ')
    satisfy (== ' ')
    name <- many1 $ satisfy isValidChar
    string " \8211 "
    place <- many1 $ satisfy isValidChar
    eof 
    return $ Entry (parseDay day) name place


isValidChar :: Char -> Bool
isValidChar c = any (c ==) validChars


validChars :: String
validChars = 
    "abcdefghijklmnopqrstuvwxyzåäö" ++
    "ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ" ++
    "0123456789" ++
    "-._~:/?#[]@!$&'()*+,;= " 


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
generateTable :: [Entry] -> IO [String]
generateTable entries = do
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
parseDay :: String -> Day
parseDay t = case (Data.List.Split.splitOn "." $ t) of
    d:(m:(y:[])) -> fromGregorian
        ((read y)::Integer)
        ((read m)::Int)
        ((read d)::Int) 
    otherwise    -> mempty


-- Perform a functor operation to a functor inside a monad.
-- Useful for various IO [Text] objects I'm bound to come across.
--
-- For example:
--
--      fmapM reverse $ readDataToList inputFile
fmapM :: (Functor f, Monad m) => (f a -> f b) -> m (f a) -> m (f b)
fmapM op a = a >>= (\b -> return $ op b)
    
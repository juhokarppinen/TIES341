{-  
    +---------------------------------------------------------------+
    | Text-to-HTML                                                  |
    | A simple Haskell project for TIES341 Functional Programming 2 |
    |                                                               |
    | By Juho Karppinen 2017                                        |
    +---------------------------------------------------------------+

    This program reads a plaintext file into a list of custom data structures 
    and outputs the contents formatted as HTML tables. 

    Non-base dependencies:

        Data.List.Split (install split)
-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Data.Time
import Data.Time.Clock
import Data.Time.Calendar
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
outputFileFuture :: String
outputFileFuture = "html/gigs-future.html"


-- Initial hardcoded filepath.
outputFilePast :: String
outputFilePast = "html/gigs-past.html"

-----------------------
-- Testing shortcuts --
h = readToList configFile
d = readToList inputFile
e = do { eD <- readToList inputFile; return $ map parse eD }
-----------------------
-----------------------

main :: IO ()
main = do
    entryData <- readToList inputFile
    today <- today
    
    let entries = map parse entryData
    let future = filterAfterDate today entries
    let past = filterBeforeDate today entries
    
    futureGigs <- generateTable future
    pastGigs <- generateTable $ reverse past
    
    writeToFile outputFileFuture futureGigs
    writeToFile outputFilePast pastGigs
    
    -- putStrLn . show $ ls -- Print output list
    -- mapM_ (putStrLn.show) entries -- Print entries
    -- mapM_ putStrLn ls -- Print output HTML


-- Return today's date.
today :: IO Day
today = getCurrentTime >>= return . utctDay


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
parseEntry = 
    do
        day <- many1 $ satisfy (/= ' ')
        satisfy (== ' ')
        name <- many1 $ satisfy isValidChar
        string " \8211 "
        place <- many1 $ satisfy isValidChar
        eof 
        return $ Entry (parseDay day) name place
    where
        isValidChar c = any (c ==) $ 
            "abcdefghijklmnopqrstuvwxyzåäöABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ" ++
            "0123456789 -–._~:/?#[]@!$&'()*+,;=\"`´’”" 


-- Return a file's contents as a list of lines.
readToList :: String -> IO [String]
readToList file = 
    do
        ls <- fmap lines (readFile file)
        return ls


-- Clears a file.
clearFile :: String -> IO ()
clearFile file = writeFile file ""


-- Appends a list of strings separated by linefeeds into a file.
appendToFile :: String -> [String] -> IO ()
appendToFile file lines = 
    do
        let lines2 = fmap (++ "\n") lines
        mapM_ (appendFile file) lines2


-- Writes a list of strings into a file.
writeToFile :: String -> [String] -> IO ()
writeToFile file lines =
    do
        clearFile file
        appendToFile file lines


-- Pretty print a list of showable IO objects.
printLines :: Show a => IO [a] -> IO ()
printLines t = 
    do
        textLines <- t
        mapM_ (putStrLn . show) textLines


-- Generate an HTML table as a list of lines. 
generateTable :: [Entry] -> IO [String]
generateTable entries = 
    do
        t0 <- fmapM (fmap (enclose "th")) $ readToList configFile
        let headerHTML = encloseL "tr" t0
        let t1 = fmap unpackEntry entries
        let t2 = fmap (fmap $ enclose "td") t1
        let entriesHTML = concat $ fmap (encloseL "tr") t2
        return $ encloseL "table" (headerHTML ++ entriesHTML)
    

-- Enclose a String in HTML tags.
enclose :: String -> String -> String
enclose tag txt = "<" ++ tag ++ ">" ++ txt ++ "</" ++ tag ++ ">"


-- Enclose a [String] in HTML tags.
encloseL :: String -> [String] -> [String]
encloseL tag txts = ["<" ++ tag ++ ">"] ++ txts ++ ["</" ++ tag ++ ">"]


-- Unpack an Entry into a [String]
unpackEntry :: Entry -> [String]
unpackEntry e = [showDateFinnishFormat $ getDate e, getName e, getPlace e]


-- Return a Day into a Finnish formatted text representation.
showDateFinnishFormat :: Day -> String
showDateFinnishFormat d = 
    Data.List.intercalate "." $ 
    Data.List.reverse $ 
    Data.List.Split.splitOn "-" $ 
    show d


-- Parse a Finnish formatted text representation of a date (d.m.y) into a Day.
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


-- Filter Entries by field comparison.
-- getter : getDate, getName or getPlace
-- comp   : method from Eq typeclass
-- val    : value to compare with 
filterEntry :: Eq a => (Entry->a) -> (a->a-> Bool) -> a -> [Entry] -> [Entry]
filterEntry getter comp val [] = []
filterEntry getter comp val (x:xs)
    | comp (getter x) val = x : (filterEntry getter comp val xs)
    | otherwise = filterEntry getter comp val xs   


-- Helper functions for filterEntry.
filterBeforeDate day e = filterEntry getDate (<) day e
filterAfterDate day e = filterEntry getDate (>=) day e
filterByPlace place e = filterEntry getPlace (==) place e
filterByName name e = filterEntry getName (==) name e


-- Amount of space characters for each indentation level.
ind :: Int
ind = 2


-- Calculate the change to indentation level caused by the line of HTML.
indentationChange :: String -> Int
indentationChange s = totalTags - closingTags * 2 where
    totalTags = length $ filter (=='<') s
    closingTags = length $ filter (=='/') s


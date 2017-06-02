module Main where

import System.IO
import System.Directory
import Data.Char        (toLower, isSpace)
import Data.Hashable    (hash)
import Numeric          (showHex)

type Choice = (String, String, IO ())

main :: IO ()
main = do
    -- dbExists <- doesFileExist "questions.txt"
    -- if not dbExists
    --     then writeFile "questions.txt" ""
    --     else return ()
    menu choices_main

menu :: [Choice] -> IO ()
menu listOfChoices = do
    putStrLn "" -- a line of separation
    mapM_ (putStrLn . ch2String) listOfChoices
    choice <- ask "input: "
    case validate listOfChoices choice of
        Just n  -> execute listOfChoices choice
        Nothing -> putStrLn "Please try again" >> menu listOfChoices

    where ch2String (i, s, _) = i ++ " - " ++ s

validate :: [Choice] -> String -> Maybe String
validate listOfChoices s
    -- let opts = map (\(c, _, _) -> [c]) choices in
    | elem s $ getOpts listOfChoices = Just s
    | otherwise                      = Nothing

getOpts :: [Choice] -> [String]
getOpts listOfChoices = map (\(c, _, _) -> c) listOfChoices

execute :: [Choice] -> String -> IO ()
execute listOfChoices n = doExec $ filter (\(i, _, _) -> i == n) listOfChoices
    where doExec ((_, _, f):_) = f -- NOTE could be done so much better?

ask :: String -> IO String
ask s = do
    putStr s
    fmap strip getLine where
        strip = f . f
        f = reverse . dropWhile isSpace

confirm :: IO () -> IO ()
confirm task = do
    input <- ask "Are you sure [y/n]? "
    if ("y"==) . map toLower $ input
        then task
        else return ()

choices_main :: [Choice]
choices_main =
    [ ("a", "Add a question", addQuestion)
    , ("sq", "Show questions", showQuestions)
    , ("sh", "Show hashes", showHashes)
    , ("q", "Quit", quit) ]


addQuestion = do
    q <- ask "Question: "
    confirm $ appendFile "questions.txt" (q ++ "\n")
    menu choices_main

showQuestions = do
    -- file <- readFile "questions.txt"
    -- ls <- return $ lines file
    -- mapM_ putStrLn ls
    readFile "questions.txt" >>= return . lines >>= (mapM_ putStrLn)
    -- readFile "questions.txt" >>= (mapM_ putStrLn) . lines
    menu choices_main

showHashes = do
    readFile "questions.txt" >>= (mapM_ putStrLn) . mappad . map (toHex . hash) . lines
    menu choices_main where
        mappad ls = map (\i -> replicate (maxLength ls - length i) '0' ++ i) ls
        maxLength = foldl max 0 . map length

toHex :: Int -> String
toHex n
    | n >= 0    = flip showHex "" n
    | otherwise = flip showHex "" $ designed n where
        designed n = (2 * (1 + toInteger (maxBound :: Int))) + toInteger (n :: Int)

quit = putStrLn "Good Day"

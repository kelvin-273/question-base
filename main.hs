module Main where

import Data.Hashable    (hash)
import Numeric          (showHex)
import Menu

main :: IO ()
main = do
    -- dbExists <- doesFileExist "questions.txt"
    -- if not dbExists
    --     then writeFile "questions.txt" ""
    --     else return ()
    menu choices_main

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

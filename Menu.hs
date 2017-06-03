module Menu
    ( Choice
    , menu
    , validate
    , getOpts
    , execute
    , ask
    , confirm
    ) where

import System.IO
import System.Directory
import Data.Char        (toLower, isSpace)

type Choice = (String, String, IO ())

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

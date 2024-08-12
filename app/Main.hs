module Main where

import System.IO ( hGetContents, IOMode(ReadMode), openFile )
import Text.Regex.TDFA
import Lexer

main :: IO ()
main = do
    handle <- openFile "resources/01.lang" ReadMode
    contents <- hGetContents handle
    putStrLn $ "Code: " ++ contents
    let tokens = tokenize contents
    printTokens tokens
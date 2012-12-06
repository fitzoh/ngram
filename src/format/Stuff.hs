module Main where
import Data.Char
import Format
import System.IO
main::IO()
main = do
        fn <- getLine
        contents <- readFile "txt/got.txt"
        putStr $ formatString contents

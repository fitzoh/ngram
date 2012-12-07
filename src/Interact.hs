module Main where
import qualified Data.Map as Map
import Data.Char
import Data.List
import System.IO
import WordBuffer
import Predictor
import Counter
import Format
import System.Exit
main::IO()
main = do
        --putStrLn "Enter a file name"
        --fn <- getLine
        contents <- readFile "txt/got.txt"
        let pred = myInterp $ formatString contents
        putStrLn "before strict"
        hSetBuffering stdin NoBuffering
        predictLoop pred eWB


predictLoop :: Predictor -> WordBuffer ->IO()
predictLoop pred wb= do
        let preds = predict pred wb 5
        putStr "\ESC[2J\n"
        print preds
        putStrLn $ showWords wb
        next <- getChar
        if next == '!'
                then exitWith ExitSuccess
                else predictLoop pred $ pickAction next preds wb
        
--select next action based on user input
pickAction :: Char -> UniPredict -> WordBuffer -> WordBuffer
pickAction next preds wb
        | next == ' ' = advanceBuffer wb
        | isDigit next = checkPredictions next preds wb
        | otherwise  = addChar wb next

--n = char to int. If n < |Predictions|, advanceword Predictions[n]
checkPredictions :: Char -> UniPredict -> WordBuffer -> WordBuffer
checkPredictions next preds wb =
        if dig >= length preds
                then wb 
                else completeWord wb newWord
        where
                newWord = fst $  preds !! (dig - 1)
                dig = digitToInt next
                
                
myInterp :: [String] -> Predictor 
myInterp formatted = predictor $ linInterpolate counter weights
        where
                counter = countWords formatted
                weights = Weights 10000 100 1

-- Advent of Code 2023
-- https://adventofcode.com/2023/day/1

{-
The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover. On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.

For example:

1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet

In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.
-}

{-
Solution:
1. Parse input into an array of strings
2. For each string, parse:
 a. From left to right, get first digit found, which will be first digit.
 b. From right to left, get first digit found, whith will be last digit.
-}

-- https://stackoverflow.com/questions/7867723/haskell-file-reading
-- https://stackoverflow.com/questions/33955026/reading-line-by-line-in-haskell
import System.IO  
import Control.Monad
-- https://stackoverflow.com/questions/4131552/haskell-check-if-integer-or-check-type-of-variable
import Data.Typeable

import Data.Char

main = do  
        let list = []
        handle <- openFile "1.txt" ReadMode
        contents <- hGetContents handle
        let singleLines = lines contents
        print singleLines
        let firstDigits = map getFirstNum singleLines
        print firstDigits
        -- We want to get the firt digit from the right, not left, so we need to reverse.
        --  (map reverse singleLines)
        {-
ghci> map reverse ["1abc2","pqr3stu8vwx","a1b2c3d4e5f","treb7uchet"]
["2cba1","xwv8uts3rqp","f5e4d3c2b1a","tehcu7bert"]
        -}
        let secondDigits = map getFirstNum (map reverse singleLines)
        print secondDigits
        let combinedDigits = combineDigits firstDigits secondDigits
        print combinedDigits
        let result = foldl (+) 0 combinedDigits
        print result
        hClose handle   

getFirstNum :: String -> Int
getFirstNum [] = -1
getFirstNum xs
  | isDigit (head xs) = digitToInt (head xs)
  | otherwise = getFirstNum (tail xs)


combineDigits :: [Int] -> [Int] -> [Int]
combineDigits [] [] = []
combineDigits l1 l2 = [10* (head l1) + (head l2)] ++ combineDigits (tail l1) (tail l2)


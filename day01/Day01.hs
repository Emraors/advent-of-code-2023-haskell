module Day01 () where

import Control.Applicative (Alternative (many), (<|>))
import Data.Attoparsec.Text
import Data.Char (isDigit)
import Data.Text as T (Text, lines, pack)
import qualified Data.Text.IO as TIO

-- Parser for each number word (It is a parser of strings and not of char because of the fucking "oneight" exception)
numberWord :: String -> Text -> Parser String
numberWord d word = d <$ string word

numberParser :: Parser String
numberParser =
  choice
    [ numberWord "18" (pack "oneight"),
      numberWord "21" (pack "twone"),
      numberWord "38" (pack "threeight"),
      numberWord "58" (pack "fiveight"),
      numberWord "79" (pack "sevenine"),
      numberWord "83" (pack "eightree"),
      numberWord "98" (pack "nineight"),
      numberWord "82" (pack "eightwo"),
      numberWord "1" (pack "one"),
      numberWord "2" (pack "two"),
      numberWord "3" (pack "three"),
      numberWord "4" (pack "four"),
      numberWord "5" (pack "five"),
      numberWord "6" (pack "six"),
      numberWord "7" (pack "seven"),
      numberWord "8" (pack "eight"),
      numberWord "9" (pack "nine")
    ]

-- Parser for digits (I added  pure to make it a parser of strings)
digitParser :: Parser String
digitParser = pure <$> satisfy isDigit

lineParser :: Parser String -> Parser String
lineParser p = concat <$> many (p <|> failureParser)
  where
    failureParser = anyChar >> return []

parseLine :: Parser String -> Text -> Either String String
parseLine p = parseOnly (lineParser p <* endOfInput)

parseInput :: (Text -> Either String String) -> Text -> [Int]
parseInput parser =
  map
    ( read
        . takeFirstAndLast
        . either error id
        . parser
    )
    . T.lines
  where
    takeFirstAndLast [] = []
    takeFirstAndLast [x] = [x, x]
    takeFirstAndLast (x : xs) = [x, last xs]

firstSolution :: IO ()
firstSolution = do
  input <- TIO.readFile "day01/day01.txt"
  let parsedInput = parseInput (parseLine digitParser) input
  print $ sum parsedInput

secondSolution :: IO ()
secondSolution = do
  input <- TIO.readFile "day01/day01.txt"
  let parsedInput =
        parseInput
          ( parseLine
              (numberParser <|> digitParser)
          )
          input
  print $ sum parsedInput

testText :: Text
testText = pack "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

secondTestText :: Text
secondTestText = pack "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day02 () where

-- import Control.Applicative (Alternative (many), (<|>))
import Data.Attoparsec.Text
import Data.Functor (($>))
import Data.Map as M (Map, fromList)
import Data.Text as T

data Color = Green | Red | Blue deriving (Show, Eq, Ord)

type RevealedCube = M.Map Color Int

type TotalCubes = M.Map Color Int

data Game = Game Int [RevealedCube] deriving (Show, Eq)

parseColor :: Parser Color
parseColor =
  choice
    [ string (pack "green") $> Green,
      string (pack "red") $> Red,
      string (pack "blue") $> Blue
    ]

pairParser :: Parser (Color, Int)
pairParser = do
  value <- decimal
  skipSpace
  key <- parseColor
  return (key, value)

-- make this a test using hspec
testPairParser :: IO ()
testPairParser = do
  print $ parseOnly pairParser (pack "1 green")
  print $ parseOnly pairParser (pack "2 red")
  print $ parseOnly pairParser (pack "3 blue")

parseRevealedCube :: Parser RevealedCube
parseRevealedCube = M.fromList <$> pairParser `sepBy` (char ',' >> skipSpace)

testParseRevealedCube :: IO ()
testParseRevealedCube = do
  print $ parseOnly parseRevealedCube (pack "1 green, 2 red, 3 blue")
  print $ parseOnly parseRevealedCube (pack "1 green, 2 red, 3 blue,")
  print $ parseOnly parseRevealedCube (pack "1 green, 2 red, 3 blue, ")

parseRevealedCubes :: Parser [RevealedCube]
parseRevealedCubes = parseRevealedCube `sepBy` (char ';' >> skipSpace)

testParseRevealedCubes :: IO ()
testParseRevealedCubes = do
  print $ parseOnly parseRevealedCubes (pack "1 green, 2 red, 3 blue; 1 green, 2 red, 3 blue")

parseGame :: Parser Game
parseGame = do
  string (pack "Game")
  skipSpace
  n <- decimal
  skip (== ':')
  skipSpace
  cubes <- parseRevealedCubes
  return $ Game n cubes

testParseGame :: IO ()
testParseGame = do
  print $ parseOnly parseGame (pack "Game 1: 1 green, 2 red, 3 blue; 1 green, 2 red, 3 blue")

isValid :: TotalCubes -> Game -> Bool
isValid = undefined

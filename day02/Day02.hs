{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day02 () where

import Data.Attoparsec.Text
import Data.Functor (($>))
import Data.Map as M (Map, fromList)
import Data.Text as T
import Test.Hspec

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

parseRevealedCube :: Parser RevealedCube
parseRevealedCube = M.fromList <$> pairParser `sepBy` (char ',' >> skipSpace)

parseRevealedCubes :: Parser [RevealedCube]
parseRevealedCubes = parseRevealedCube `sepBy` (char ';' >> skipSpace)

parseGame :: Parser Game
parseGame = do
  string (pack "Game")
  skipSpace
  n <- decimal
  skip (== ':')
  skipSpace
  cubes <- parseRevealedCubes
  return $ Game n cubes

isValid :: TotalCubes -> Game -> Bool
isValid = undefined

-- | TESTS
tests :: IO ()
tests = do
  testPairParser
  testParseRevealedCube
  testParseRevealedCubes
  testParseGame

testPairParser :: IO ()
testPairParser = hspec $ do
  describe "pairParser" $ do
    it "parses 1 green" $ do
      parseOnly pairParser (pack "1 green") `shouldBe` Right (Green, 1)
    it "parses 2 red" $ do
      parseOnly pairParser (pack "2 red") `shouldBe` Right (Red, 2)
    it "parses 3 blue" $ do
      parseOnly pairParser (pack "3 blue") `shouldBe` Right (Blue, 3)

testParseRevealedCube :: IO ()
testParseRevealedCube = hspec $ do
  describe "parseRevealedCube" $ do
    it "parses 1 green, 2 red,  3  blue" $ do
      parseOnly
        parseRevealedCube
        (pack "1 green, 2 red,  3  blue")
        `shouldBe` Right (M.fromList [(Green, 1), (Red, 2), (Blue, 3)])
    it "parses 1 green, 2 red, 3 blue, " $ do
      parseOnly
        parseRevealedCube
        (pack "1 green, 2 red, 3 blue, ")
        `shouldBe` Right (M.fromList [(Green, 1), (Red, 2), (Blue, 3)])

testParseRevealedCubes :: IO ()
testParseRevealedCubes = hspec $ do
  describe "parseRevealedCubes" $ do
    it "parses 1 green, 2 red, 3 blue; 1 green, 2 red, 3 blue" $ do
      parseOnly
        parseRevealedCubes
        (pack "1 green, 2 red, 3 blue; 1 green, 2 red, 3 blue")
        `shouldBe` Right
          [ M.fromList [(Green, 1), (Red, 2), (Blue, 3)],
            M.fromList [(Green, 1), (Red, 2), (Blue, 3)]
          ]

testParseGame :: IO ()
testParseGame = hspec $ do
  describe "parseGame" $ do
    it "parses Game 1: 1 green, 2 red, 3 blue; 1 green, 2 red, 3 blue" $ do
      parseOnly
        parseGame
        (pack "Game 1: 1 green, 2 red, 3 blue; 1 green, 2 red, 3 blue")
        `shouldBe` Right
          ( Game
              1
              [ M.fromList [(Green, 1), (Red, 2), (Blue, 3)],
                M.fromList [(Green, 1), (Red, 2), (Blue, 3)]
              ]
          )

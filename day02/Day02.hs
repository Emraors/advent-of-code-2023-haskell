{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day02 () where

import Data.Attoparsec.Text
import Data.Functor (($>))
import Data.Map as M
  ( Map,
    empty,
    findWithDefault,
    foldrWithKey,
    fromList,
    unionWith,
  )
import Data.Text as T (Text, lines, pack)
import Test.Hspec (describe, hspec, it, shouldBe)

-- | TYPES
data Color = Green | Red | Blue deriving (Show, Eq, Ord)

type RevealedCube = M.Map Color Int

type TotalCubes = M.Map Color Int

data Game = Game Int [RevealedCube] deriving (Show, Eq)

-- | PARSERS
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

parseInput :: Text -> [Game]
parseInput =
  map
    ( either error id
        . parseOnly parseGame
    )
    . T.lines

-- | FIRST PART
totalCubes :: TotalCubes
totalCubes = M.fromList [(Green, 13), (Red, 12), (Blue, 14)]

isValid :: TotalCubes -> RevealedCube -> Bool
isValid tc = M.foldrWithKey check True
  where
    check key val acc = acc && (M.findWithDefault 0 key tc >= val)

isGameValid :: TotalCubes -> Game -> Bool
isGameValid tc (Game _ cubes) = all (isValid tc) cubes

sumID :: [Game] -> Int
sumID = sum . map (\(Game n _) -> n)

firstSolution :: IO ()
firstSolution = do
  input <- readFile "day02/day02.txt"
  let games =
        filter
          (isGameValid totalCubes)
          (parseInput (pack input))
  print $ sumID games

-- | SECOND PART
maxCubes :: [RevealedCube] -> RevealedCube
maxCubes = foldr (M.unionWith max) M.empty

powCubes :: RevealedCube -> Int
powCubes = product

secondSolution :: IO ()
secondSolution = do
  input <- readFile "day02/day02.txt"
  let games = parseInput (pack input)
  print $ sum $ map (\(Game _ cubes) -> powCubes . maxCubes $ cubes) games

-- | TESTS
tests :: IO ()
tests = do
  testPairParser
  testParseRevealedCube
  testParseRevealedCubes
  testParseGame
  testIsValid
  testIsGameValid
  testSumID
  testMaxCubes
  testPowCubes

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

testIsValid :: IO ()
testIsValid = hspec $ do
  describe "isValid" $ do
    it "returns True for valid cubes" $ do
      isValid totalCubes (M.fromList [(Green, 1), (Red, 2), (Blue, 3)])
        `shouldBe` True
    it "returns False for invalid cubes" $ do
      isValid totalCubes (M.fromList [(Green, 20), (Red, 2), (Blue, 4)])
        `shouldBe` False

testIsGameValid :: IO ()
testIsGameValid = hspec $ do
  describe "isGameValid" $ do
    it "returns True for valid game" $ do
      isGameValid
        totalCubes
        ( Game
            1
            [ M.fromList [(Green, 1), (Red, 2), (Blue, 3)],
              M.fromList [(Green, 1), (Red, 2), (Blue, 3)]
            ]
        )
        `shouldBe` True
    it "returns False for invalid game" $ do
      isGameValid
        totalCubes
        ( Game
            1
            [ M.fromList [(Green, 20), (Red, 2), (Blue, 3)],
              M.fromList [(Green, 1), (Red, 2), (Blue, 3)]
            ]
        )
        `shouldBe` False

testSumID :: IO ()
testSumID = hspec $ do
  describe "sumID" $ do
    it "returns 2 for 2 games" $ do
      sumID
        [ Game
            1
            [ M.fromList [(Green, 1), (Red, 2), (Blue, 3)],
              M.fromList [(Green, 1), (Red, 2), (Blue, 3)]
            ],
          Game
            1
            [ M.fromList [(Green, 1), (Red, 2), (Blue, 3)],
              M.fromList [(Green, 1), (Red, 2), (Blue, 3)]
            ]
        ]
        `shouldBe` 2

testMaxCubes :: IO ()
testMaxCubes = hspec $ do
  describe "maxCubes" $ do
    it "returns 1 green, 4 red, 3 blue" $ do
      maxCubes
        [ M.fromList [(Green, 1), (Red, 2), (Blue, 3)],
          M.fromList [(Green, 0), (Blue, 0)],
          M.fromList [(Green, 0), (Red, 4), (Blue, 0)]
        ]
        `shouldBe` M.fromList [(Green, 1), (Red, 4), (Blue, 3)]
    it "returns 1 green, 2 red, 3 blue for 1 green, 2 red, 3 blue; 1 green, 2 red, 3 blue" $ do
      maxCubes
        [ M.fromList [(Green, 1), (Red, 2), (Blue, 3)],
          M.fromList [(Green, 1), (Red, 2), (Blue, 3)]
        ]
        `shouldBe` M.fromList [(Green, 1), (Red, 2), (Blue, 3)]

testPowCubes :: IO ()
testPowCubes = hspec $ do
  describe "powCubes" $ do
    it "returns 6 for 1 green, 2 red, 3 blue" $ do
      powCubes (M.fromList [(Green, 1), (Red, 2), (Blue, 3)]) `shouldBe` 6
    it "returns 6 for 1 green, 2 red, 3 blue; 1 green, 2 red, 3 blue" $ do
      powCubes
        ( M.fromList [(Green, 1), (Blue, 3)]
        )
        `shouldBe` 3

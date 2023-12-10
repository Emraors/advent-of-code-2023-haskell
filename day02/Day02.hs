{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day02 () where

import Data.Attoparsec.Text
import Data.Functor (($>))
import Data.Map as M (Map, findWithDefault, foldrWithKey, fromList)
import Data.Text as T (Text, lines, pack)
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

initTotalCubes :: TotalCubes
initTotalCubes = M.fromList [(Green, 13), (Red, 12), (Blue, 14)]

-- not yet sure if it should be gretter or equal or just greater
isValid :: TotalCubes -> RevealedCube -> Bool
isValid totalCubes = M.foldrWithKey check True
  where
    check key val acc = acc && (M.findWithDefault 0 key totalCubes >= val)

isGameValid :: TotalCubes -> Game -> Bool
isGameValid totalCube (Game _ cubes) = all (isValid totalCube) cubes

sumID :: [Game] -> Int
sumID = sum . map (\(Game n _) -> n)

parseInput :: Parser Game -> Text -> [Game]
parseInput parser =
  map
    ( either error id
        . parseOnly parser
    )
    . T.lines

firstSolution :: IO ()
firstSolution = do
  input <- readFile "day02/day02.txt"
  let games =
        filter
          (isGameValid initTotalCubes)
          (parseInput parseGame (pack input))
  print $ sumID games

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
      isValid initTotalCubes (M.fromList [(Green, 1), (Red, 2), (Blue, 3)])
        `shouldBe` True
    it "returns False for invalid cubes" $ do
      isValid initTotalCubes (M.fromList [(Green, 20), (Red, 2), (Blue, 4)])
        `shouldBe` False

testIsGameValid :: IO ()
testIsGameValid = hspec $ do
  describe "isGameValid" $ do
    it "returns True for valid game" $ do
      isGameValid
        initTotalCubes
        ( Game
            1
            [ M.fromList [(Green, 1), (Red, 2), (Blue, 3)],
              M.fromList [(Green, 1), (Red, 2), (Blue, 3)]
            ]
        )
        `shouldBe` True
    it "returns False for invalid game" $ do
      isGameValid
        initTotalCubes
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

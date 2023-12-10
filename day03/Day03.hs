{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day03 () where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char (isDigit)
import Data.Functor
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text as T (Text, lines, pack)
import Test.Hspec (describe, hspec, it, shouldBe)

type Coordinate = (Int, Int)

{-
data MatrixElement = MatrixElement
  { coordinate :: Coordinate,
    value :: Value
  }
  deriving (Show, Eq)
-}
data Value = Symbol Char | Integer Int | Dot deriving (Show, Eq)

type Matrix = M.Map Coordinate Value

type Dimensions = (Int, Int)

-- Parser for a Value
valueParser :: Parser Value
valueParser =
  (Integer . read <$> many1 digit)
    <|> (Symbol <$> satisfy (\c -> not (isDigit c) && c /= '.'))
    <|> (char '.' $> Dot)

testValueParser :: IO ()
testValueParser = hspec $
  describe "valueParser" $ do
    it "parses values" $ do
      parseOnly valueParser (pack "12") `shouldBe` Right (Integer 12)
      parseOnly valueParser (pack "a") `shouldBe` Right (Symbol 'a')
      parseOnly valueParser (pack ".") `shouldBe` Right Dot

rowParser :: Parser [Value]
rowParser = many valueParser

testRowParser :: IO ()
testRowParser = hspec $
  describe "rowParser" $ do
    it "parses rows" $ do
      parseOnly
        rowParser
        (pack "12a.3%\n")
        `shouldBe` Right [Integer 12, Symbol 'a', Dot, Integer 3, Symbol '%']

parseMatrix :: Text -> [[Value]]
parseMatrix text = map (either error id . parseOnly rowParser) (T.lines text)

testInput :: Text
testInput =
  pack
    "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."

testParseMatrix :: IO ()
testParseMatrix =
  hspec $
    describe "parseMatrix" $
      it "parses a matrix" $
        parseMatrix (pack "12a.3%\n2345..")
          `shouldBe` [ [Integer 12, Symbol 'a', Dot, Integer 3, Symbol '%'],
                       [Integer 2345, Dot, Dot]
                     ]

-- | Returns the value at the given coordinate
valueAt :: Matrix -> Coordinate -> Maybe Value
valueAt m coordinate = M.lookup coordinate m

nearestNeighbours :: Matrix -> Coordinate -> [Value]
nearestNeighbours m (x, y) =
  catMaybes
    [ valueAt m (x + dx, y + dy)
      | dx <- [-1 .. 1],
        dy <- [-1 .. 1],
        not (dx == x && dy == y)
    ]

matrix :: Matrix
matrix =
  M.fromList
    [ ((0, 0), Integer 1),
      ((1, 0), Integer 2),
      ((0, 1), Integer 3),
      ((1, 1), Integer 4)
    ]

testNearestNeighbours :: IO ()
testNearestNeighbours =
  hspec $
    describe "nearestNeighbours" $
      it "returns the nearest neighbours" $
        nearestNeighbours matrix (0, 0)
          `shouldBe` [Integer 3, Integer 2, Integer 4]

listToMatrix :: [[Value]] -> Matrix
listToMatrix values =
  M.fromList $
    concat $
      zipWith rowToCoordValue [0 ..] values
  where
    rowToCoordValue rowNum =
      zipWith
        (\colNum val -> ((rowNum, colNum), val))
        [0 ..]

testListToMatrix :: IO ()
testListToMatrix =
  hspec $
    describe "listToMatrix" $
      it "converts a list of lists to a matrix" $
        listToMatrix
          [ [Integer 1, Integer 3],
            [Integer 2, Integer 4]
          ]
          `shouldBe` matrix

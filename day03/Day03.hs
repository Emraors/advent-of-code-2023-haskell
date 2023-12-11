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

data Value
  = Symbol Char
  | Digit Char
  | Dot
  deriving (Show, Eq)

type Matrix = M.Map Coordinate Value

type Dimensions = (Int, Int)

valueParser :: Parser Value
valueParser =
  (Digit <$> digit)
    <|> (Symbol <$> satisfy (\c -> not (isDigit c) && c /= '.'))
    <|> (char '.' $> Dot)

testValueParser :: IO ()
testValueParser = hspec $
  describe "valueParser" $ do
    it "parses values" $ do
      parseOnly valueParser (pack "1") `shouldBe` Right (Digit '1')
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
        (pack "1a.3%")
        `shouldBe` Right
          [ Digit '1',
            Symbol 'a',
            Dot,
            Digit '3',
            Symbol '%'
          ]

parseMatrix :: Text -> [[Value]]
parseMatrix text =
  map
    ( either error id
        . parseOnly rowParser
    )
    (T.lines text)

testInput :: Text
testInput =
  pack
    "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."

testParseMatrix :: IO ()
testParseMatrix =
  hspec $
    describe "parseMatrix" $
      it "parses a matrix" $
        parseMatrix (pack "1a.3%\n2..%.")
          `shouldBe` [ [ Digit '1',
                         Symbol 'a',
                         Dot,
                         Digit '3',
                         Symbol '%'
                       ],
                       [ Digit '2',
                         Dot,
                         Dot,
                         Symbol '%',
                         Dot
                       ]
                     ]

elementAt :: Matrix -> Coordinate -> Maybe MatrixElement
elementAt m coordinate = (,) coordinate <$> M.lookup coordinate m

type MatrixElement = (Coordinate, Value)

nearestNeighbours :: Matrix -> Coordinate -> [MatrixElement]
nearestNeighbours m (x, y) =
  catMaybes
    [ elementAt m (x + dx, y + dy)
      | dx <- [-1 .. 1],
        dy <- [-1 .. 1],
        not (dx == x && dy == y)
    ]

matrix :: Matrix
matrix =
  M.fromList
    [ ((0, 0), Digit '1'),
      ((1, 0), Digit '2'),
      ((0, 1), Digit '3'),
      ((1, 1), Digit '4')
    ]

testNearestNeighbours :: IO ()
testNearestNeighbours =
  hspec $
    describe "nearestNeighbours" $
      it "returns the nearest neighbours" $
        nearestNeighbours matrix (0, 0)
          `shouldBe` [ ((1, 0), Digit '2'),
                       ((0, 1), Digit '3'),
                       ((1, 1), Digit '4')
                     ]

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
          [ [Digit '1', Digit '3'],
            [Digit '2', Digit '4']
          ]
          `shouldBe` matrix

extractDigits :: Matrix -> [MatrixElement] -> [Int]
extractDigits = undefined

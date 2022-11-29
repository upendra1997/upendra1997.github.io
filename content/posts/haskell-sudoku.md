---
title: "Haskell Sudoku"
date: 2022-11-29T11:14:10+05:30
# weight: 1
# aliases: ["/first"]
tags: ["first"]
categories: ["category"]
author: "Upendra Upadhyay"
# author: ["Me", "You"] # multiple authors
showToc: true
TocOpen: true
draft: true
hidemeta: false
comments: true
description: "Desc Text."
canonicalURL: "https://canonical.url/to/page"
disableHLJS: true # to disable highlightjs
disableShare: false
disableHLJS: false
hideSummary: false
searchHidden: false
ShowReadingTime: true
ShowBreadCrumbs: true
ShowPostNavLinks: true
cover:
    image: "<image path/url>" # image path/url
    alt: "<alt text>" # alt text
    caption: "<text>" # display caption under cover
    relative: false # when using page bundles set this to true
    hidden: true # only hide on current single page
editPost:
    URL: "https://github.com/upendra1997.github.io/content"
    Text: "Suggest Changes" # edit text
    appendFilePath: true # to append file path to Edit link
---

Last time, I mentioned that I tried to write sudoku solver in haskell and it was using too much memory and time. So I tried to solve it again and this time with few years of studying haskell, I was successful.


`Main.hs`
```haskell
module Main (main) where

import Data.Char (digitToInt)
import Control.Monad (replicateM)
import Lib (Cell, Row, sudokuSize, getSolutions, generateSudoku, showSudoku)

parseChar :: Char -> Cell
parseChar '.' = 0
parseChar x = digitToInt x

parseString :: String -> Row
parseString = map parseChar

readRow :: IO Row
readRow = do
  row <- parseString <$> getLine
  if length row /= sudokuSize
    then error "row does not have correct number of cells"
    else return row

main :: IO ()
main = do
    sudoku <- replicateM sudokuSize readRow
    mapM_ (putStrLn . showSudoku) $ generateSudoku sudoku
```



`Lib.hs`
```haskell
module Lib
  ( Cell,
    Row,
    Sudoku,
    sudokuSize,
    getSolutions,
    showSudoku,
    generateSudoku
  )
where

import           Control.Monad (guard)
import           Data.List     (nub, sort, transpose, (\\))

type Cell = Int

type SudokuSize = Int

sudokuSize :: SudokuSize
sudokuSize = 9

blockSize :: Int
blockSize = 3

showRow :: Show a => [a] -> String
showRow row = unwords $ show <$> row

showSudoku :: Show a => [[a]] -> String
showSudoku sudoku = unlines $ showRow <$> sudoku

type Row = [Int]

type X = Int

type Y = Int

type Coord = (X, Y)

type Sudoku = [Row]

possibilities :: Cell -> [Cell]
possibilities 0 = [1 .. 9]
possibilities n = [n]

crossProduct :: [[a]] -> [[a]]
crossProduct []             = []
crossProduct [a]            = [[x] | x <- a]
crossProduct (array : rest) = (:) <$> array <*> crossProduct rest

-- similar function exist called sequence which is:
-- sequence :: Monad m => [m a] -> m [a]
-- sequence = foldr mcons (return [])

-- mcons :: Monad m => m t -> m [t] -> m [t]
-- mcons p q = do
--   x <- p
--   y <- q
--   return (x:y)

rows :: [[a]] -> [[[a]]]
rows = fmap (replicate sudokuSize)

columns :: [[a]] -> [[[a]]]
columns = transpose . rows . transpose

coordinates :: [[Coord]]
coordinates = [[(r, c) | c <- [0 .. sudokuSize - 1]] | r <- [0 .. sudokuSize - 1]]

blockCoordinates :: [[(Coord, Coord)]]
blockCoordinates = (fmap . fmap) (\(x, y) -> (start x y, end x y)) coordinates
  where
    start x' y' = (3 * (x' `div` blockSize), 3 * (y' `div` blockSize))
    end x' y' = (\(x'', y'') -> (x'' + blockSize, y'' + blockSize)) $ start x' y'

slice :: Int -> Int -> [a] -> [a]
slice start end = drop start . take end

slice2D :: [[a]] -> Int -> Int -> Int -> Int -> [[a]]
slice2D sudoku startRow endRow startCol endCol = slice startRow endRow $ slice startCol endCol <$> sudoku

blocks :: Sudoku -> [[[[Int]]]]
blocks sudoku = (fmap . fmap) block blockCoordinates
  where
    getSlice = slice2D sudoku
    block ((startRow, startCol), (endRow, endCol)) = getSlice startRow endRow startCol endCol

getAllValues :: Sudoku -> [[[Cell]]]
getAllValues sudoku = (fmap . fmap) (sort . nub) all'
  where
    add' = (zipWith . zipWith) (++)
    all' = add' blocks' $ add' rows' cols'
    rows' = rows sudoku
    cols' = columns sudoku
    blocks' = fmap concat <$> blocks sudoku

valid :: Sudoku -> Bool
valid sudoku = (all . all) (== 9) (fmap length <$> getAllValues sudoku)


possibilitiesWithContext :: Sudoku -> Coord -> [Cell]
possibilitiesWithContext sudoku coord = if currentValue == 0 then possibleValues else [currentValue]
  where (x, y) = coord
        currentValue = head $ concat $ slice2D sudoku x (x + 1) y (y + 1)
        allValues' = getAllValues sudoku
        allValues = concatMap concat $ slice2D allValues' x (x + 1) y (y + 1)
        possibleValues = [0..sudokuSize] \\ allValues

-- getSolutions :: Sudoku -> [Sudoku]
-- getSolutions sudoku = filter valid allSudokus
--   where
--     allSudokus = crossProduct (crossProduct . fmap possibilities <$> sudoku)

-- ghci> product $ fmap product $ fmap (length . possibilities) <$> sudoku
-- 8599843895832833305

getSolutions :: Sudoku -> [Sudoku]
getSolutions sudoku = filter valid allSudokus
  where
    possibilities' = possibilitiesWithContext sudoku
    allSudokus = crossProduct (crossProduct . fmap possibilities' <$> coordinates)

-- ghci> product $ fmap product $ fmap (length . possibilities') <$> coordinates
-- 2972033482752
-- almost 1000000 times fewer searches have to be done for this

-- sudoku :: Sudoku
-- sudoku = [[9, 2, 6, 3, 4, 0, 7, 0, 1],
--           [0, 5, 0, 0, 2, 6, 4, 0, 9],
--           [0, 7, 0, 8, 0, 1, 0, 0, 0],
--           [0, 0, 0, 9, 0, 0, 2, 0, 7],
--           [3, 4, 2, 0, 0, 0, 0, 0, 5],
--           [1, 0, 0, 0, 0, 0, 8, 0, 0],
--           [6, 8, 5, 4, 0, 0, 0, 1, 2],
--           [0, 0, 4, 0, 0, 2, 9, 0, 0],
--           [0, 1, 0, 5, 3, 8, 0, 7, 0]]

-- sudoku2 :: Sudoku
-- sudoku2 = [[2, 9, 5, 7, 4, 3, 8, 6, 1],
--            [4, 3, 1, 8, 6, 5, 9, 0, 0],
--            [8, 7, 6, 1, 9, 2, 5, 4, 3],
--            [3, 8, 7, 4, 5, 9, 2, 1, 6],
--            [6, 1, 2, 3, 8, 7, 4, 9, 5],
--            [5, 4, 9, 2, 1, 6, 7, 3, 8],
--            [7, 6, 3, 5, 2, 4, 1, 8, 9],
--            [9, 2, 8, 6, 7, 1, 3, 5, 4],
--            [1, 5, 4, 9, 3, 8, 6, 0, 0]]

-- emptySudoku :: Sudoku
-- emptySudoku = [[0, 0, 0, 0, 0, 0, 0, 0, 0],
--                [0, 0, 0, 0, 0, 0, 0, 0, 0],
--                [0, 0, 0, 0, 0, 0, 0, 0, 0],
--                [0, 0, 0, 0, 0, 0, 0, 0, 0],
--                [0, 0, 0, 0, 0, 0, 0, 0, 0],
--                [0, 0, 0, 0, 0, 0, 0, 0, 0],
--                [0, 0, 0, 0, 0, 0, 0, 0, 0],
--                [0, 0, 0, 0, 0, 0, 0, 0, 0],
--                [0, 0, 0, 0, 0, 0, 0, 0, 0]]


replaceAt :: Int -> (a -> a) -> [a] -> [a]
replaceAt index f array = left ++ (f current : right')
  where (left, right) = splitAt index array
        current = head right
        right' = tail right

generateSudoku :: Sudoku -> [Sudoku]
generateSudoku sudoku = f coords' sudoku
  where coords' = concatMap (fmap filter id ifValid) coordinates -- concat $ (fmap.fmap filter id) ifValid coordinates
        ifValid (x, y) = 0 == (sudoku !! x !! y)
        f [] sudoku' = do
          guard (valid sudoku')
          return sudoku'
        f (coord: coords) sudoku' =  do
          let (x, y) = coord
          let values = possibilitiesWithContext sudoku' coord
          guard $ (not . null) values
          -- traceM ("(x, y, values) \r" ++ show (coord, values))
          val <- values
          let sudoku'' = replaceAt x (replaceAt y (const val)) sudoku'
          -- traceM ("sudoku \r" ++ showSudoku sudoku'')
          f coords sudoku''
```

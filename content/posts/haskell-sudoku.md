---
title: "Haskell Sudoku"
date: 2022-11-29T11:14:10+05:30
# weight: 1
# aliases: ["/first"]
tags: ["first"]
categories: ["Haskell"]
author: "Upendra Upadhyay"
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

Last time, I mentioned that I tried to write sudoku solver in haskell and it was using too much memory and time. So I tried to solve it again and I was able to do it, I guess this time the different thing was that I am more experienced.

We will be using this file format as an input to our suodku program:

```
92634.7.1
.5..264.9
.7.8.1...
...9..2.7
342.....5
1.....8..
6854...12
..4..29..
.1.538.7.
```
and it will be represented as an array of numbers and `.` will be represented as `0`, so for our repl the suokdu will be `[[Int]]`

```haskell
[[9, 2, 6, 3, 4, 0, 7, 0, 1],
 [0, 5, 0, 0, 2, 6, 4, 0, 9],
 [0, 7, 0, 8, 0, 1, 0, 0, 0],
 [0, 0, 0, 9, 0, 0, 2, 0, 7],
 [3, 4, 2, 0, 0, 0, 0, 0, 5],
 [1, 0, 0, 0, 0, 0, 8, 0, 0],
 [6, 8, 5, 4, 0, 0, 0, 1, 2],
 [0, 0, 4, 0, 0, 2, 9, 0, 0],
 [0, 1, 0, 5, 3, 8, 0, 7, 0]]
```

above sudoku only have one solution, but there could be sudokus with multiple solutions also, like:

``` haskell
[[2, 9, 5, 7, 4, 3, 8, 6, 1],
 [4, 3, 1, 8, 6, 5, 9, 0, 0],
 [8, 7, 6, 1, 9, 2, 5, 4, 3],
 [3, 8, 7, 4, 5, 9, 2, 1, 6],
 [6, 1, 2, 3, 8, 7, 4, 9, 5],
 [5, 4, 9, 2, 1, 6, 7, 3, 8],
 [7, 6, 3, 5, 2, 4, 1, 8, 9],
 [9, 2, 8, 6, 7, 1, 3, 5, 4],
 [1, 5, 4, 9, 3, 8, 6, 0, 0]]
```

or we could have an empty sudoku, which will give us all the valid sudokus possible in the world:

```haskell
[[0, 0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0, 0]]
```

## Project Structure

This is created by using `stack new suokdu`
```
.
├── CHANGELOG.md
├── LICENSE
├── README.md
├── Setup.hs
├── app
│   └── Main.hs
├── emptySudoku.txt   -- empty suodku input file.
├── package.yaml
├── src
│   └── Lib.hs
├── stack.yaml
├── stack.yaml.lock
├── sudoku.cabal
├── sudoku.txt        -- only one solution sudoku file.
└── test
    └── Spec.hs
```


## Defining Types
here we will be defining few of the types which will be using in our program, all of them are type aliases `type` insted of `newtype` as I wanted to use theses type interchangebly with genric functions like `crossProduct`, which we will see later.

```haskell
-- src/Lib.hs

type Cell = Int

type SudokuSize = Int

type Row = [Int]

type X = Int

type Y = Int

type Coord = (X, Y) -- Coordinates

type Sudoku = [Row]
```

## Config
We will be storing some config like `sudokuSize` and the `gridSize` in the Sudoku in the `Lib.hs` file.

```haskell
-- src/Lib.hs
sudokuSize :: SudokuSize
sudokuSize = 9

blockSize :: Int
blockSize = 3
```

## Parsing and printing Suodku

We need to pretty print our sudoku in terminal, so for that we will be defining `showSudoku` function:
```haskell
-- src/Lib.hs

showRow :: Show a => [a] -> String
showRow row = unwords $ show <$> row

showSudoku :: Show a => [[a]] -> String
showSudoku sudoku = unlines $ showRow <$> sudoku
```

and to parse our sudoku from file we will define below functions:

```haskell
-- app/main.hs

module Main (main) where

import Data.Char (digitToInt)
import Control.Monad (replicateM)
import Lib (Cell, Row, sudokuSize, showSudoku)

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
    mapM_ (putStrLn . showSudoku) [sudoku]
```

We have definied a funciton `readRow` which will read from `stdin` and try to parse it as a `Row`, and since `readRow` is an `IO`, i.e. it reads from terminal, we can use `replicateM` to repeat this operation and read multiple rows from the terminal.

if we look at the type of replicateM in the `stack repl`, we will see that:

```haskell
ghci> :t replicateM
replicateM :: Applicative m => Int -> m a -> m [a]
```
so here `(Int -> m a) -> m [a]` means that it will collect `a` from `m` computations and will return a new computation `m [a]` where all the `a` have been gathered.
i.e.
so here `(9 -> readRow)` replicateM needs a number `9` and a computation to replicate which is `readRow`, and it will return `IO [Row]` which is basically a sudoku.

***

similar funciton in `MapM`, where _ will ignore the result, and we want that `main` function type is `IO ()`,

```haskell
ghci> :t mapM
mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
ghci> :t mapM_
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
```
## Sudoku Solutions

### getting solutions wihout context

```haskell
-- src/Lib.hs

possibilities :: Cell -> [Cell]
possibilities 0 = [1 .. 9]
possibilities n = [n]

crossProduct :: [[a]] -> [[a]]
crossProduct []             = []
crossProduct [a]            = [[x] | x <- a]
crossProduct (array : rest) = (:) <$> array <*> crossProduct rest


getSolutions :: Sudoku -> [Sudoku]
getSolutions sudoku = filter valid allSudokus
  where
    allSudokus = crossProduct (crossProduct . fmap possibilities <$> sudoku)

```


so for this kind of solution we have around these many possibilities:

```haskell
ghci> product $ fmap product $ fmap (length . possibilities) <$> sudoku
8599843895832833305
```

### getting solutions based on context
### generating solutions based on context


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

---
title: "Haskell Sudoku"
date: 2022-11-29T11:14:10+05:30
weight: 2
# aliases: ["/first"]
tags: ["haskell"]
categories: ["Haskell"]
author: "Upendra Upadhyay"
showToc: true
TocOpen: true
draft: false
hidemeta: false
comments: true
description: "A sudoku solver in Haskell."
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
    URL: "https://github.com/upendra1997/upendra1997.github.io/blob/main/content"
    Text: "Suggest Changes" # edit text
    appendFilePath: true # to append file path to Edit link
---

Last time, I mentioned that I tried to write sudoku solver in haskell and it was using too much memory and time. So I tried to solve it again and this time I was able to do it, I guess I just needed more motivation.

We will be using this file format as input to our suodku program:

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
and it will be represented as an array of numbers and `.` will be represented as `0`, so for our repl, the sudoku will be `[[Int]]`

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
 [4, 3, 1, 8, 6, 5, 9, 0, 0],  -- 2 7
 [8, 7, 6, 1, 9, 2, 5, 4, 3],
 [3, 8, 7, 4, 5, 9, 2, 1, 6],
 [6, 1, 2, 3, 8, 7, 4, 9, 5],
 [5, 4, 9, 2, 1, 6, 7, 3, 8],
 [7, 6, 3, 5, 2, 4, 1, 8, 9],
 [9, 2, 8, 6, 7, 1, 3, 5, 4],
 [1, 5, 4, 9, 3, 8, 6, 0, 0]]  -- 7 2
```  

or we could have empty sudoku, which will give us all the valid sudokus possible in the world:

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
├── emptySudoku.txt   -- empty sudoku input file.
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
here we will be defining a few of the types which will be used in our program, all of them are type aliases `type` instead of `newtype` as I wanted to use these types interchangeably with generic functions like `crossProduct`, which we will see later.

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
We will be storing some configs like `sudokuSize` and the `gridSize` in the Sudoku in the `Lib.hs` file.

```haskell
-- src/Lib.hs

sudokuSize :: SudokuSize
sudokuSize = 9

blockSize :: Int
blockSize = 3
```

## Parsing and printing Sudoku

We need to pretty print our sudoku in the terminal, so for that, we will be defining `showSudoku` function:
```haskell
-- src/Lib.hs

showRow :: Show a => [a] -> String
showRow row = unwords $ show <$> row

showSudoku :: Show a => [[a]] -> String
showSudoku sudoku = unlines $ showRow <$> sudoku
```

and to parse our sudoku from the file we will define below the functions:

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
    then error "row does not have the correct number of cells"
    else return row

main :: IO ()
main = do
    sudoku <- replicateM sudokuSize readRow
    mapM_ (putStrLn . showSudoku) [sudoku]
```

We have defined a function `readRow` which will read from `stdin` and try to parse it as a `Row`, and since `readRow` is an `IO`, i.e. it reads from the terminal, we can use `replicateM` to repeat this operation and read multiple rows from the terminal.

if we look at the type of replicateM in the `stack repl`, we will see that:

```haskell
ghci> :t replicateM
replicateM :: Applicative m => Int -> m a -> m [a]
```
so here `(Int -> m a) -> m [a]` means that it will collect `a` from `m` type of computation and will return a new computation `m [a]` where all the `a` have been gathered from different computations.
i.e.
so here `(9 -> readRow)` replicateM needs a number `9` and a computation to replicate which is `readRow`, and it will return `IO [Row]` which is a sudoku.


similarly in `MapM` function, where _ will ignore the result:

```haskell
ghci> :t mapM
mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
ghci> :t mapM_
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
```

## Validate Sudokus

for a sudoku to be valid, each row and column must contain a number from 1 to 9 exactly once; and also there are 9 grids of size `3x3` in a `9x9` sudoku, where no number must repeat.

### get all rows

```haskell
-- src/lib.hs

rows :: [[a]] -> [[[a]]]
rows = fmap (replicate sudokuSize)
```

### get all columns

```haskell
-- src/lib.hs

columns :: [[a]] -> [[[a]]]
columns = transpose . rows . transpose
```
### get all grid blocks

to get all grid blocks we need to know which grid each element of sudoku belongs to, every grid has a start and end position which can be represented by `Coord` type.


Coordinates of all elements in sudoku:
```haskell
-- src/lib.hs

coordinates :: [[Coord]]
coordinates = [[(r, c) | c <- [0 .. sudokuSize - 1]] | r <- [0 .. sudokuSize - 1]]
```

block coordinate will give us the start and end position pairs of all sudoku elements.


```haskell
-- src/lib.hs

blockCoordinates :: [[(Coord, Coord)]]
blockCoordinates = (fmap . fmap) (\(x, y) -> (start x y, end x y)) coordinates
  where
    start x' y' = (3 * (x' `div` blockSize), 3 * (y' `div` blockSize))
    end x' y' = (\(x'', y'') -> (x'' + blockSize, y'' + blockSize)) $ start x' y'
```

if Sudoku type was parametrized like:
```haskell
type Sudoku a = [[a]]
```
then we could have represented `blockCoordinates` as:
```haskell
type Rectangle = (Coord, Coord) -- (start, end)
blockCoordinates :: [[Rectangle]]
```

to get values from block coordinates we need a few helper functions like `slice` and `slice2D`

```haskell
-- src/lib.hs

slice :: Int -> Int -> [a] -> [a]
slice start end = drop start . take end
```

```haskell
-- src/lib.hs

slice2D :: [[a]] -> Int -> Int -> Int -> Int -> [[a]]
slice2D sudoku startRow endRow startCol endCol = slice startRow endRow $ slice startCol endCol <$> sudoku
```

and finally, our function to get blocks at each coordinate:

```haskell
-- src/lib.hs

blocks :: Sudoku -> [[[[Int]]]]
blocks sudoku = (fmap . fmap) block blockCoordinates
  where
    getSlice = slice2D sudoku
    block ((startRow, startCol), (endRow, endCol)) = getSlice startRow endRow startCol endCol
```

to understand it better, lets take our previous case where Sudoku was parametrized, which would give us

```haskell
blocks :: Sudoku -> Sudoku (Sudoku Int)
```
here blocks is like a sudoku of sudokus, where inner sudoku is a `3x3` grid.

finally, we merge all the values of the rows, column and grid values at each coordinate of the grid and we get:

```haskell
-- src/lib.hs

getAllValues :: Sudoku -> [[[Cell]]]
getAllValues sudoku = (fmap . fmap) (sort . nub) all'
  where
    add' = (zipWith . zipWith) (++)
    all' = add' blocks' $ add' rows' cols'
    rows' = rows sudoku
    cols' = columns sudoku
    blocks' = fmap concat <$> blocks sudoku
```
and alternate way would to see this would be if `Suodku` was parametrized
```haskell
getAllValues :: Sudoku -> Sudoku [Int]
``` 
this tells us that each sudoku element is an array of integers.

and by this scenario, we can easily validate sudoku if all the elements of sudoku contain exactly 9 values
```haskell
-- src/lib.hs

valid :: Sudoku -> Bool
valid sudoku = (all . all) (== 9) (fmap length <$> getAllValues sudoku)
```

## Sudoku Solutions

### getting solutions without context

The easiest solution would be to try 1 to 9 values for each 0 in the sudoku.
the time complexity of this would be `9^n` where n is the number of 0 in the sudoku.
We already know that this solution is very slow, and we will never get any answer. 
But let's see how it would be implemented in haskell.


first, we need a helper function that would give us all possible values for each element:
```haskell
-- src/Lib.hs

possibilities :: Cell -> [Cell]
possibilities 0 = [1 .. 9]
possibilities n = [n]
```

then we need a way to cross-produce each possibility.
Here cross-product is the same as the cross-product of a set in mathematics.

```haskell
ghci> crossProduct [[1, 2, 3], [4, 5], [6]]
[[1,4,6],[1,5,6],[2,4,6],[2,5,6],[3,4,6],[3,5,6]]
```

and here is its implementation:
```haskell
-- src/Lib.hs

crossProduct :: [[a]] -> [[a]]
crossProduct []             = []
crossProduct [a]            = [[x] | x <- a]
crossProduct (array : rest) = (:) <$> array <*> crossProduct rest
```
we could also use the sequence function from Prelude, which does the same thing.
```haskell
ghci> :t sequence
sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
ghci> sequence [[1,2,3], [4,5], [6]]
[[1,4,6],[1,5,6],[2,4,6],[2,5,6],[3,4,6],[3,5,6]]
```
So by getting a cross-product of each possibility we will get all solutions available.

```haskell
-- src/Lib.hs

getSolutions :: Sudoku -> [Sudoku]
getSolutions sudoku = filter valid allSudokus
  where
    allSudokus = crossProduct (crossProduct . fmap possibilities <$> sudoku)
```

so for this kind of solution, we have these many possibilities:

```haskell
ghci> product $ fmap product $ fmap (length . possibilities) <$> sudoku
8599843895832833305
```
Which is a lot...

### getting solutions based on context

now, we can be smarter about this and only generate possibilities which are not already present in the row, column and grid blocks.

```haskell
-- src/Lib.hs

possibilitiesWithContext :: Sudoku -> Coord -> [Cell]
possibilitiesWithContext sudoku coord = if currentValue == 0 then possibleValues else [currentValue]
  where (x, y) = coord
        currentValue = head $ concat $ slice2D sudoku x (x + 1) y (y + 1)                          -- sudoku !! x !! y
        allValues' = getAllValues sudoku
        allValues = concatMap concat $ slice2D allValues' x (x + 1) y (y + 1)
        possibleValues = [0..sudokuSize] \\ allValues                                              -- subtract a from b | `a` \\ `b`
```

and will be plugging it in the getSolutions approach.

```haskell
-- src/Lib.hs

getSolutions :: Sudoku -> [Sudoku]
getSolutions sudoku = filter valid allSudokus
  where
    possibilities' = possibilitiesWithContext sudoku
    allSudokus = crossProduct (crossProduct . fmap possibilities' <$> coordinates)
```

```haskell
ghci> product $ fmap product $ fmap (length . possibilities') <$> coordinates
2972033482752
```

for this solution, almost 1000000 times fewer searches have to be done for this; but still slow for our computer.

### generating solutions based on context

We can have a faster solution if for each coordinate with `0` we take one possible number and try to generate possibilities for other holes in the sudoku, if at some point we reach a hole in the sudoku where there is no possible number, then we **backtrack** to the previous hole and try next possibility, once we are done will all the holes in the sudoku, we return our solution.

for this approach, we would need a helper function that would replace our sudoku with given coordinates and a new value.

```haskell
-- src/Lib.hs

replaceAt :: Int -> (a -> a) -> [a] -> [a]                -- works for 1D array
replaceAt index f array = left ++ (f current : right')
  where (left, right) = splitAt index array
        current = head right
        right' = tail right
```

and so finally our solution

```haskell
-- src/Lib.hs

generateSudoku :: [Coord] -> Sudoku -> [Sudoku]
generateSudoku [] sudoku' = do
          guard (valid sudoku')
          return sudoku'
generateSudoku (coord: coords) sudoku' =  do
  let (x, y) = coord
  let values = possibilitiesWithContext sudoku' coord
  guard $ (not . null) values
  val <- values
  let sudoku'' = replaceAt x (replaceAt y (const val)) sudoku'
  generateSudoku coords sudoku''
```

Let's break it down.
`coords` is a list of coordinates where we have holes = `0`.
`generateSudoku` is a function that will try to replace `coord` with a possibility and will backtrack if no possibility is present at some point using `guard`.

and let's plug it into our solution

```haskell
-- src/Lib.hs

getSolutions :: Sudoku -> [Sudoku]
getSolutions sudoku = generateSudoku coords' sudoku
  where coords' = concatMap (fmap filter id ifValid) coordinates -- concat $ (fmap.fmap filter id) ifValid coordinates
        ifValid (x, y) = 0 == (sudoku !! x !! y)
```

## Printing all solutions

so finally our main function looks like:
```haskell
-- app/Main.hs

main :: IO ()
main = do
    sudoku <- replicateM sudokuSize readRow
    mapM_ (putStrLn . showSudoku) $ getSolutions sudoku
```

and our solution is
```
9 2 6 3 4 5 7 8 1
8 5 1 7 2 6 4 3 9
4 7 3 8 9 1 5 2 6
5 6 8 9 1 3 2 4 7
3 4 2 6 8 7 1 9 5
1 9 7 2 5 4 8 6 3
6 8 5 4 7 9 3 1 2
7 3 4 1 6 2 9 5 8
2 1 9 5 3 8 6 7 4
```

***

Github Repository: [https://github.com/upendra1997/sudoku-haskell](https://github.com/upendra1997/sudoku-haskell)

***

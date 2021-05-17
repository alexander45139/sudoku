module Sudoku where

import Data.Char (digitToInt)
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Data.List (transpose, group, sort, elemIndex, nub)
import Data.List.Split (chunksOf)

-------------------------------------------------------------------------

{-| A Sudoku puzzle is a list of lists, where each value is a Maybe Int. That is,
each value is either `Nothing' or `Just n', for some Int value `n'. |-}
newtype Puzzle = Puzzle [[Maybe Int]]
 deriving (Show, Eq)

{-| A Block is a list of 9 Maybe Int values. Each Block represents a row, a column,
or a square. |-}
type Block = [Maybe Int]

{-| A Pos is a zero-based (row, column) position within the puzzle. |-}
newtype Pos = Pos (Int, Int) deriving (Show, Eq)

{-| A getter for the rows in a Sudoku puzzle. |-}
rows :: Puzzle -> [[Maybe Int]]
rows (Puzzle rs) = rs

example :: Puzzle
example =
  Puzzle
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

{-| Ex 1.1

    A sudoku with just blanks. |-}
allBlankPuzzle :: Puzzle
allBlankPuzzle = Puzzle (map (\r -> map (\m -> Nothing) [1..9]) [1..9])

{-| Ex 1.2

    Checks if sud is really a valid representation of a sudoku puzzle. |-}
isPuzzle :: Puzzle -> Bool
isPuzzle (Puzzle rs) = let numRows        = length rs
                           numColsAsList  = map (length) rs
                           isNumColsAll9s = and (map (==9) numColsAsList) in
                       if numRows == 9 && isNumColsAll9s
                       then let valuesBetween1And9 = map (\r -> map (\m -> case m of
                                                                             Just n  -> n > 0 && n < 10
                                                                             Nothing -> True) r) rs
                                isRowsTrue         = map (\r -> and r) valuesBetween1And9
                            in and isRowsTrue
                       else False

{-| Ex 1.3

    Checks if the puzzle is already solved, i.e. there are no blanks. |-}
isSolved :: Puzzle -> Bool
isSolved (Puzzle rs) = let eachRowOfNothingVals = map (\r -> all (/= Nothing) r) rs
                       in and eachRowOfNothingVals

{-| Ex 2.1

    `printPuzzle s' prints a representation of `s'. |-}
printPuzzle :: Puzzle -> IO ()
printPuzzle (Puzzle rs) = let puzzleAsTextForm       = map (\r -> map (\m -> case m of
                                                                               Just n  -> show n
                                                                               Nothing -> ".") r) rs
                              puzzleAsTextFormConcat = map (\row -> concat row) puzzleAsTextForm
                          in putStrLn (unlines puzzleAsTextFormConcat)

{-| Ex 2.2

    `readPuzzle f' reads from the FilePath `f', and either delivers it, or stops
    if `f' did not contain a puzzle. |-}
readPuzzle :: FilePath -> IO Puzzle
readPuzzle f = do content <- readFile f
                  let rs = map (\r -> map (\m -> case m of
                                                    '.' -> Nothing
                                                    n   -> Just (digitToInt n)) r) (lines content)
                      p = Puzzle rs
                  if isPuzzle p then pure p else error "This is not a puzzle!"

{-| Ex 3.1

    Check that a block contains no duplicate values. |-}
isValidBlock :: Block -> Bool
isValidBlock b = let bWithoutNothingValues              = filter (/= Nothing) b
                     bWithoutNothingValuesAndDuplicates = nub bWithoutNothingValues
                 in (length bWithoutNothingValues == length bWithoutNothingValuesAndDuplicates)

{-| Ex 3.2

    Collect all blocks on a board - the rows, the columns and the squares. |-}
blocks :: Puzzle -> [Block]
blocks (Puzzle rs) = let columns                 = transpose rs
                         rowsOf3s                = map (\r -> chunksOf 3 r) rs
                         columnsOf3s             = transpose rowsOf3s
                         firstHalfOfblocksOf3x3  = map (\c -> concat (take 3 c)) columnsOf3s
                         secondHalfOfBlocksOf3x3 = map (\c -> concat (take 3 (drop 3 c))) columnsOf3s
                         thirdHalfOfBlocksOf3x3  = map (\c -> concat (drop 6 c)) columnsOf3s
                         blocksOf3x3             = firstHalfOfblocksOf3x3 ++ secondHalfOfBlocksOf3x3 ++ thirdHalfOfBlocksOf3x3
                     in rs ++ columns ++ blocksOf3x3

{-| Ex 3.3

    Check that all blocks in a puzzle are legal. |-}
isValidPuzzle :: Puzzle -> Bool
isValidPuzzle p = and (map (\b -> isValidBlock b) (blocks p))

{-| Ex 4.1

    Given a Puzzle that has not yet been solved, returns a position in
    the Puzzle that is still blank. If there are more than one blank
    position, you may decide yourself which one to return. |-}
blank :: Puzzle -> Pos
blank (Puzzle rs) = let indexesOfNothingValueInEachRow                      = map (\r -> elemIndex Nothing r) rs
                        indexOfNothingValueInFirstRowWithANothingValue      = head (filter (/=Nothing) indexesOfNothingValueInEachRow)
                        indexOfNothingValueInFirstRowWithANothingValueAsInt = case indexOfNothingValueInFirstRowWithANothingValue of Just n -> n
                        firstIndexOfRowContainingANothingValue              = elemIndex indexOfNothingValueInFirstRowWithANothingValue indexesOfNothingValueInEachRow
                        firstIndexOfRowContainingANothingValueAsInt         = case firstIndexOfRowContainingANothingValue of Just n -> n
                    in Pos (firstIndexOfRowContainingANothingValueAsInt, indexOfNothingValueInFirstRowWithANothingValueAsInt)
                    

{-| Ex 4.2

    Given a list, and a tuple containing an index in the list and a
    new value, updates the given list with the new value at the given
    index. |-}
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] (_, _) = []
(!!=) xs (p, v) = if p >= 0 && p < length xs then (take p xs) ++ [v] ++ (drop (p + 1) xs) else xs

{-| Ex 4.3

    `update s p v' returns a puzzle which is a copy of `s' except that
    the position `p' is updated with the value `v'. |-}
update :: Puzzle -> Pos -> Maybe Int -> Puzzle
update (Puzzle rs) (Pos (r,i)) v = let row             = rs !! r
                                       rowWithNewValue = row !!= (i, v)
                                   in Puzzle ((take r rs) ++ [rowWithNewValue] ++ (drop (r + 1) rs))

{-| Ex 5.1

    Solve the puzzle. |-}
solve :: Puzzle -> Maybe Puzzle
solve p = if isSolved p then Just p
          else if isPuzzle p && isValidPuzzle p
               then let pos                                               = blank p
                        indexOfRowInBlocks                                = case pos of Pos (r, i) -> r
                        indexOfcolumnInBlocks                             = case pos of Pos (r, i) -> i + 9
                        indexOfblockOf3x3InBlocks                         = case pos of Pos (r, i) -> if i < 3 then r + 18 else if i > 2 && i < 6 then r + 19 else r + 20
                        allValuesToCompare                                = concat ([blocks p !! indexOfRowInBlocks] ++ [blocks p !! indexOfcolumnInBlocks] ++ [blocks p !! indexOfblockOf3x3InBlocks])
                        allValuesToCompareWithoutNothingValsAndDuplicates = nub (filter (/=Nothing) allValuesToCompare)
                        possibleValues                                    = drop (length allValuesToCompareWithoutNothingValsAndDuplicates) (nub (allValuesToCompareWithoutNothingValsAndDuplicates ++ [Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9]))
                    in solve (update p pos (head possibleValues))
               else Nothing
               
          

{-| Ex 5.2

    Read a puzzle and solve it. |-}
readAndSolve :: FilePath -> IO (Maybe Puzzle)
readAndSolve f = do puzzle <- readPuzzle f
                    printPuzzle (fromJust (solve puzzle))

{-| Ex 5.3

    Checks if s1 is a solution of s2. |-}
isSolutionOf :: Puzzle -> Puzzle -> Bool
isSolutionOf s1 s2 = solve s2 == Just s1


module Sudoku where

import Debug.Trace
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
allBlankPuzzle = Puzzle (map (\r -> map (\m -> Nothing) [1..9]) [1..9])  -- creates a list of 9 lists with each containing 9 Nothing values

{-| Ex 1.2

    Checks if sud is really a valid representation of a sudoku puzzle. |-}
isPuzzle :: Puzzle -> Bool
isPuzzle (Puzzle rs) = let numRows        = length rs
                           numColsAsList  = map (length) rs  -- creates a list where each value represents the number of values in each row
                           isNumColsAll9s = and (map (==9) numColsAsList) in  -- gives a bool values to represent whether each row in the puzzle has 9 values
                       if numRows == 9 && isNumColsAll9s  -- if the puzzle has 9 rows of 9 values
                       then let valuesBetween1And9 = map (\r -> map (\m -> case m of
                                                                             Just n  -> n > 0 && n < 10
                                                                             Nothing -> True) r) rs  -- creates a list of lists with bool values to represent whether each value in the puzzle lies between 1 and 9
                                isRowsTrue         = map (\r -> and r) valuesBetween1And9  -- creates a list of bool values where True is 'all values in this row are between 1 and 9'
                            in and isRowsTrue  -- returns True if all the values in isRowsTrue are True, meaning all values in each row are between 1 and 9
                       else False

{-| Ex 1.3

    Checks if the puzzle is already solved, i.e. there are no blanks. |-}
isSolved :: Puzzle -> Bool
isSolved (Puzzle rs) = let eachRowOfNothingVals = map (\r -> all (/= Nothing) r) rs  -- creates a list of bool values where False values mean that there are at least Nothing values in this row
                       in and eachRowOfNothingVals  -- returns True if eachRowOfNothingVals contains all True values, saying whether there are no Nothing values in rs

{-| Ex 2.1

    `printPuzzle s' prints a representation of `s'. |-}
printPuzzle :: Puzzle -> IO ()
printPuzzle (Puzzle rs) = let puzzleAsTextForm       = map (\r -> map (\m -> case m of
                                                                               Just n  -> show n
                                                                               Nothing -> ".") r) rs  -- creates of list of lists with each containing a set of characters representing a number of a value in the given puzzle or a "." as a blank
                              puzzleAsTextFormConcat = map (\row -> concat row) puzzleAsTextForm  -- creates a list of string values where each represents a row of numbers and blanks (e.g. "136.2.784")
                          in putStrLn (unlines puzzleAsTextFormConcat)  -- prints out the previous variable as a string where each list item seperated by a new line

{-| Ex 2.2

    `readPuzzle f' reads from the FilePath `f', and either delivers it, or stops
    if `f' did not contain a puzzle. |-}
readPuzzle :: FilePath -> IO Puzzle
readPuzzle f = do content <- readFile f
                  let rs = map (\r -> map (\m -> case m of
                                                    '.' -> Nothing
                                                    n   -> Just (digitToInt n)) r) (lines content)  -- converts the read content of printed characters into a list of rows with Maybe Int values
                      p = Puzzle rs
                  if isPuzzle p then pure p else error "This is not a puzzle!"  -- checks whether p is a Puzzle return it as pure Puzzle value or return an error

{-| Ex 3.1

    Check that a block contains no duplicate values. |-}
isValidBlock :: Block -> Bool
isValidBlock b = let bWithoutNothingValues              = filter (/= Nothing) b  -- filters out the Nothing values from the block (list)
                     bWithoutNothingValuesAndDuplicates = nub bWithoutNothingValues  -- duplicate values are removed
                 in (length bWithoutNothingValues == length bWithoutNothingValuesAndDuplicates)  -- returns True if there are no duplicate values in b by comparing lengths of these two lists

{-| Ex 3.2

    Collect all blocks on a board - the rows, the columns and the squares. |-}
blocks :: Puzzle -> [Block]
blocks (Puzzle rs) = let columns                 = transpose rs  -- gives the columns of this list of rows
                         rowsOf3s                = map (\r -> chunksOf 3 r) rs  -- creates a list of lists of 3 separated groups of 3 values
                         columnsOf3s             = transpose rowsOf3s
                         firstSectionOfBlocksOf3x3  = map (\c -> concat (take 3 c)) columnsOf3s
                         secondSectionOfBlocksOf3x3 = map (\c -> concat (take 3 (drop 3 c))) columnsOf3s
                         thirdSectionOfBlocksOf3x3  = map (\c -> concat (drop 6 c)) columnsOf3s
                         blocksOf3x3             = firstSectionOfBlocksOf3x3 ++ secondSectionOfBlocksOf3x3 ++ thirdSectionOfBlocksOf3x3  -- a list of lists containing 9 values that represent each block of 3 by 3 in the puzzle
                     in rs ++ columns ++ blocksOf3x3  -- returns a list of rows, columns and blocks of 3 by 3's

{-| Ex 3.3

    Check that all blocks in a puzzle are legal. |-}
isValidPuzzle :: Puzzle -> Bool
isValidPuzzle p = and (map (\b -> isValidBlock b) (blocks p))  -- returns True if the produced list of blocks from p are all valid

{-| Ex 4.1

    Given a Puzzle that has not yet been solved, returns a position in
    the Puzzle that is still blank. If there are more than one blank
    position, you may decide yourself which one to return. |-}
blank :: Puzzle -> Pos
blank (Puzzle rs) = let indexesOfNothingValueInEachRow                      = map (\r -> elemIndex Nothing r) rs
                        indexOfNothingValueInFirstRowWithANothingValue      = head (filter (/=Nothing) indexesOfNothingValueInEachRow) --trace ("indexesOfNothingValueInEachRow: "++(show indexesOfNothingValueInEachRow)) $ head (filter (/=Nothing) indexesOfNothingValueInEachRow)
                        indexOfNothingValueInFirstRowWithANothingValueAsInt = case indexOfNothingValueInFirstRowWithANothingValue of Just n -> n
                        firstIndexOfRowContainingANothingValue              = elemIndex indexOfNothingValueInFirstRowWithANothingValue indexesOfNothingValueInEachRow
                        firstIndexOfRowContainingANothingValueAsInt         = case firstIndexOfRowContainingANothingValue of Just n -> n
                    in Pos (firstIndexOfRowContainingANothingValueAsInt, indexOfNothingValueInFirstRowWithANothingValueAsInt)
                    

{-| Ex 4.2

    Given a list, and a tuple containing an index in the list and a
    new value, updates the given list with the new value at the given
    index. |-}
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] (_, _) = []  -- empty values = no values to be replaced
(!!=) xs (p, v) = if p >= 0 && p < length xs then (take p xs) ++ [v] ++ (drop (p + 1) xs) else xs  -- takes all the values up to the given position and appends the new value to that and appends all the values after the given position

{-| Ex 4.3

    `update s p v' returns a puzzle which is a copy of `s' except that
    the position `p' is updated with the value `v'. |-}
update :: Puzzle -> Pos -> Maybe Int -> Puzzle
update (Puzzle rs) (Pos (r,i)) v = let row             = rs !! r  -- get list of row given at position r
                                       rowWithNewValue = row !!= (i, v)  -- updates the new value in the row
                                   in Puzzle ((take r rs) ++ [rowWithNewValue] ++ (drop (r + 1) rs))  -- puzzle returned with replaced row

{-| Ex 5.1

    Solve the puzzle. |-}
solve :: Puzzle -> Maybe Puzzle
solve p = if isSolved p then Just p  -- return the Puzzle if solved
          else if isPuzzle p && isValidPuzzle p  -- checks the puzzle is legal
               then let pos                                               = blank p  -- next blank position
                        allValuesToCompare                                = valsAroundPos pos p  -- list of all the values associated with the current blank value
                        allValuesToCompareWithoutNothingValsAndDuplicates = nub (filter (/=Nothing) allValuesToCompare)
                        possibleValues                                    = drop (length allValuesToCompareWithoutNothingValsAndDuplicates) (nub (allValuesToCompareWithoutNothingValsAndDuplicates ++ [Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9]))  -- all values between 1 and 9 that are not the associated values
                    in solve (update p pos (head possibleValues)) -- calls this function again with this blank position replaced with a Just value
               else Nothing
               
valsAroundPos :: Pos -> Puzzle -> [Maybe Int]
valsAroundPos (Pos (r,i)) (Puzzle rs) = rs !! r ++ transpose rs !! i ++ concat bl
        where bl | r < 3     = let b = map (\r -> take 3 r) rs in if i < 3 then take 3 b else if i < 6 then take 3 (drop 3 b) else drop 6 b
                 | r < 6     = let b = map (\r -> take 3 (drop 3 r)) rs in if i < 3 then take 3 b else if i < 6 then take 3 (drop 3 b) else drop 6 b
                 | otherwise = let b = map (\r -> drop 6 r) rs in if i < 3 then take 3 b else if i < 6 then take 3 (drop 3 b) else drop 6 b

{-| Ex 5.2

    Read a puzzle and solve it. |-}
readAndSolve :: FilePath -> IO (Maybe Puzzle)
readAndSolve f = do puzzle <- readPuzzle f
                    return (solve puzzle)

{-| Ex 5.3

    Checks if s1 is a solution of s2. |-}
isSolutionOf :: Puzzle -> Puzzle -> Bool
isSolutionOf s1 s2 = solve s2 == Just s1  -- returns True if s1 is the same as the returned value of solve s2


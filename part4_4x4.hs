import Data.Array

main = do
    let solution = solve puzzleBoard
    printBoard solution

-- The marks on the board are represented by Ints in the range 0..9, where 0 represents "empty".
type Mark = Int

-- A square is identified by a (row, column) pair
type Location = (Int, Int)

-- A sudoku board is a 9x9 matrix of marks
type Board = Array Location Mark

-- The sudoku board to be solved
puzzleBoard :: Board
puzzleBoard = array ((0, 0), (3, 3)) $ puzzleAssocs examplePuzzle

-- Example puzzle from http://en.wikipedia.org/wiki/Sudoku
examplePuzzle :: [[Mark]]
examplePuzzle = [[1, 0, 0, 2],
                 [0, 0, 0, 3],

                 [0, 4, 0, 0],
                 [0, 0, 0, 0]]

-- Return first solution, or Nothing if no solutions found
solve :: Board -> Maybe Board
solve = headOrNothing . solutions

-- Return all solutions
solutions :: Board -> [Board]
solutions b = solutions' (emptyLocations b) b
  where
    -- Given list of empty locations on a board, pick an empty location,
    -- determine which marks can be put in that location, and then
    -- recursively find all solutions for that set of marks.
    solutions' :: [Location] -> Board -> [Board]
    solutions' []     b = [b]
    solutions' (x:xs) b = concatMap (solutions' xs) candidateBoards
      where
        candidateMarks  = [m | m <- [1..4], isPossibleMark m x b]
        candidateBoards = map (\m -> copyWithMark m x b) candidateMarks

-- Return list of locations where value is 0
emptyLocations :: Board -> [Location]
emptyLocations b = [(row, col) | row <- [0..3], col <- [0..3], b ! (row, col) == 0]

-- Determine whether the specified mark can be placed at specified position
isPossibleMark :: Mark -> Location -> Board -> Bool
isPossibleMark m (row, col) b = notInRow && notInColumn && notInBox
  where
    notInRow    = notElem m $ b `marksInRow` row
    notInColumn = notElem m $ b `marksInColumn` col
    notInBox    = notElem m $ b `marksIn3x3Box` (row, col)

-- Return board with specified value in specified Location
copyWithMark :: Mark -> Location -> Board -> Board
copyWithMark mark (row, col) b = b // [((row, col), mark)]

-- Return the marks in the specified row
marksInRow :: Board -> Int -> [Mark]
b `marksInRow` row = [b ! loc | loc <- range((row, 0), (row, 3))]

-- Return the marks in the specified column
marksInColumn ::  Board -> Int -> [Mark]
b `marksInColumn` col = [b ! loc | loc <- range((0, col), (3, col))]

-- Return the marks in the 3x3 box that includes the specified Location
marksIn3x3Box :: Board -> Location -> [Mark]
b `marksIn3x3Box` (row, col) = [b ! loc | loc <- locations]
  where
    row' = (row `div` 2) * 2
    col' = (col `div` 2) * 2
    locations = range((row', col'), (row' + 1, col' + 1))

-- Convert a list of rows of marks (as in examplePuzzle above) to a list of array associations
puzzleAssocs :: [[Mark]] -> [(Location, Mark)]
puzzleAssocs = concatMap rowAssocs . zip [0..3]
  where
    rowAssocs :: (Int, [Mark]) -> [((Int, Int), Mark)]
    rowAssocs (row, marks) = colAssocs row $ zip [0..3] marks

    colAssocs :: Int -> [(Int, Mark)] -> [((Int, Int), Mark)]
    colAssocs row cols = map (\(col, m) -> ((row, col), m)) cols

headOrNothing :: [a] -> Maybe a
headOrNothing []     = Nothing
headOrNothing (x:xs) = Just x

printBoard :: Maybe Board -> IO ()
printBoard Nothing  = putStrLn "No solution"
printBoard (Just b) = mapM_ putStrLn [show $ b `marksInRow` row | row <- [0..3]]
import Data.Array
import Data.List

{-
Utility function to read words in Int type. 
-}
parseIntList :: String -> [Int]
parseIntList s = [read x :: Int | x <- words s]


{-
Just an example matrix walkthrough,
To understand by example working of each method.
-}
-- [[1, 0, 0, 2],
--  [0, 0, 0, 3],
--  [0, 4, 0, 0],
--  [0, 0, 0, 0]]


-- [((0,0),1),((0,1),0),((0,2),0),((0,3),2),
--  ((1,0),0),((1,1),0),((1,2),0),((1,3),3),
--  ((2,0),0),((2,1),4),((2,2),0),((2,3),0),
--  ((3,0),0),((3,1),0),((3,2),0),((3,3),0)]

{-
Basically create an array (For easy access) using sudokuAssocs
-}
sudokuBoard :: [[Int]]-> (Array (Int, Int) Int)
sudokuBoard x = array ((0, 0), (8, 8)) $ concatMap attatchRows $ zip [0,1..8] x
  where
    attatchRows :: (Int, [Int]) -> [((Int, Int), Int)]
    attatchRows (r, m) = attatchCols r $ zip [0,1..8] m

    attatchCols :: Int -> [(Int, Int)] -> [((Int, Int), Int)]
    attatchCols r c = map (\(c, m) -> ((r, c), m)) c

{-
The Main Function,
Computes all the solutinons possible
returns a list of accepted solutions.
-}
solve :: (Array (Int, Int) Int) -> [(Array (Int, Int) Int)]
solve b = solve' [(r, c) | c <- [0,1..8], r <- [0,1..8], b ! (r, c) == 0] b
  where
    solve' :: [(Int,Int)] -> (Array (Int, Int) Int) -> [(Array (Int, Int) Int)]
    solve' []     b = [b]
    solve' (x:xs) b = concatMap (solve' xs) candidateBoards
      where
        candidateInts  = [m | m <- [1,2..9], try m x b]
        candidateBoards = map (\m -> b // [(x, m)]) candidateInts

{-
try a mark M at a position
if same mark exists in that row, return false
if same mark exists in that col, return false
if same mark exists in that 2x2 subgrix, return false
Other wise return true.
-}
try :: Int -> (Int,Int) -> (Array (Int, Int) Int) -> Bool
try m (r, c) b = tryRow && tryCol && trySubgrid
  where
    tryRow = notElem m usedRowElems
    tryCol = notElem m usedColElems
    trySubgrid = notElem m usedSubgridElems
    usedRowElems = [b ! x | x <- range((r, 0), (r, 8))]
    usedColElems = [b ! x | x <- range((0, c), (8, c))]
    usedSubgridElems = [b ! loc | loc <- locations]
      where
        row' = (r `div` 3) * 3
        col' = (c `div` 3) * 3
        locations = range((row', col'), (row' + 2, col' + 2))


{-
[] : returns Nothing
[1,2,3] : returns 1
-}
safetyHelper :: [a] -> Maybe a
safetyHelper []     = Nothing
safetyHelper (x:xs) = Just x

{-
Prints in a format that is more readable. 
If no solution is found, print "No solution"
-}
printSudoku :: Maybe (Array (Int, Int) Int) -> IO ()
printSudoku Nothing  = putStrLn "No solution"
printSudoku (Just b) = mapM_ putStrLn [show $ [b ! x | x <- range((row, 0), (row, 8))] | row <- [0..8]]


main = do
    a1 <- getLine
    let intlist1 = parseIntList a1
    a2 <- getLine
    let intlist2 = parseIntList a2
    a3 <- getLine
    let intlist3 = parseIntList a3
    a4 <- getLine
    let intlist4 = parseIntList a4
    a5 <- getLine
    let intlist5 = parseIntList a5
    a6 <- getLine
    let intlist6 = parseIntList a6
    a7 <- getLine
    let intlist7 = parseIntList a7
    a8 <- getLine
    let intlist8 = parseIntList a8
    a9 <- getLine
    let intlist9 = parseIntList a9
    
    -- [[1, 0, 0, 2],
    --  [0, 0, 0, 3],
    --  [0, 4, 0, 0],
    --  [0, 0, 0, 0]]
    let inListFinal = [intlist1, intlist2, intlist3, intlist4, intlist5, intlist6, intlist7, intlist8, intlist9]
    
    let l1 = length intlist1
    let l2 = length intlist2
    let l3 = length intlist3
    let l4 = length intlist4
    let l5 = length intlist5
    let l6 = length intlist6
    let l7 = length intlist7
    let l8 = length intlist8
    let l9 = length intlist9

    let min1 = minimum intlist1
    let max1 = maximum intlist1
    let min2 = minimum intlist2
    let max2 = maximum intlist2
    let min3 = minimum intlist3
    let max3 = maximum intlist3
    let min4 = minimum intlist4
    let max4 = maximum intlist4
    let min5 = minimum intlist5
    let max5 = maximum intlist5
    let min6 = minimum intlist6
    let max6 = maximum intlist6
    let min7 = minimum intlist7
    let max7 = maximum intlist7
    let min8 = minimum intlist8
    let max8 = maximum intlist8
    let min9 = minimum intlist9
    let max9 = maximum intlist9


    if ((l1 /= 9) || (l2 /= 9) || (l3 /= 9) || (l4 /= 9) || (l5 /= 9) || (l6 /= 9) || (l7 /= 9) || (l8 /= 9) || (l9 /= 9))
        then putStrLn "Input Format Wrong"
    else if (not ((min1 >= 0 && max1 <= 4) && (min2 >= 0 && max2 <= 4) && (min3 >= 0 && max3 <= 4) && (min4 >= 0 && max4 <= 4) && (min5 >= 0 && max5 <= 4) && (min6 >= 0 && max6 <= 4) && (min7 >= 0 && max7 <= 4) && (min8 >= 0 && max8 <= 4) && (min9 >= 0 && max9 <= 4)))
        then putStrLn "Input Entry Wrong"
    else do
        let solution = safetyHelper.solve $ sudokuBoard inListFinal
        printSudoku solution
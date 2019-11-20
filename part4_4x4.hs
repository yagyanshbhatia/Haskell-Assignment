import Data.Array
import Data.List

{-
Utility function to read words in Int type. 
-}
parseIntList :: String -> [Int]
parseIntList s = [read x :: Int | x <- words s]


{-
Utility function to count number of occurences of an element in a list. 
-}
countElems :: Int -> [Int] -> Int
countElems _ []     = 0
countElems n (x:xs) = fromEnum (n == x) + countElems n xs


{-
Utility function to count number of occurences of an element in a list. 
-}
totalCount :: Int -> [Int] -> [Int] -> [Int] -> [Int] -> Int
totalCount a w x y z = countElems a w + countElems a x + countElems a y + countElems a z


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
sudokuBoard x = array ((0, 0), (3, 3)) $ concatMap attatchRows $ zip [0,1,2,3] x
  where
    attatchRows :: (Int, [Int]) -> [((Int, Int), Int)]
    attatchRows (r, m) = attatchCols r $ zip [0,1,2,3] m

    attatchCols :: Int -> [(Int, Int)] -> [((Int, Int), Int)]
    attatchCols r c = map (\(c, m) -> ((r, c), m)) c


{-
The Main Function,Computes all the solutinons possible returns a list of accepted solutions.
-}
solve :: (Array (Int, Int) Int) -> [(Array (Int, Int) Int)]
solve b = solve' [(r, c) | c <- [0,1,2,3], r <- [0,1,2,3], b ! (r, c) == 0] b
  where
    solve' :: [(Int,Int)] -> (Array (Int, Int) Int) -> [(Array (Int, Int) Int)]
    solve' []     b = [b]
    solve' (x:xs) b = concatMap (solve' xs) candidateBoards
      where
        candidateInts  = [m | m <- [1,2,3,4], try m x b]
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
    usedRowElems = [b ! x | x <- range((r, 0), (r, 3))]
    usedColElems = [b ! x | x <- range((0, c), (3, c))]
    usedSubgridElems = [b ! loc | loc <- locations]
      where
        row' = (r `div` 2) * 2
        col' = (c `div` 2) * 2
        locations = range((row', col'), (row' + 1, col' + 1))


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
printSudoku (Just b) = mapM_ putStrLn [show $ [b ! x | x <- range((row, 0), (row, 3))] | row <- [0..3]]


main = do
    a1 <- getLine
    let intlist1 = parseIntList a1
    a2 <- getLine
    let intlist2 = parseIntList a2
    a3 <- getLine
    let intlist3 = parseIntList a3
    a4 <- getLine
    let intlist4 = parseIntList a4
    
    -- [[1, 0, 0, 2],
    --  [0, 0, 0, 3],
    --  [0, 4, 0, 0],
    --  [0, 0, 0, 0]]
    let inListFinal = [intlist1, intlist2, intlist3, intlist4]
    
    let l1 = length intlist1
    let l2 = length intlist2
    let l3 = length intlist3
    let l4 = length intlist4

    let min1 = minimum intlist1
    let max1 = maximum intlist1
    let min2 = minimum intlist2
    let max2 = maximum intlist2
    let min3 = minimum intlist3
    let max3 = maximum intlist3
    let min4 = minimum intlist4
    let max4 = maximum intlist4

    let count1 = totalCount 1 intlist1 intlist2 intlist3 intlist4
    let count2 = totalCount 2 intlist1 intlist2 intlist3 intlist4
    let count3 = totalCount 3 intlist1 intlist2 intlist3 intlist4
    let count4 = totalCount 4 intlist1 intlist2 intlist3 intlist4

    if ((l1 /= 4) || (l2 /= 4) || (l3 /= 4) || (l4 /= 4))
        then putStrLn "Length of input lists should be exactly 4."
    else if (not ((min1 >= 0 && max1 <= 4) && (min2 >= 0 && max2 <= 4) && (min3 >= 0 && max3 <= 4) && (min4 >= 0 && max4 <= 4)))
        then putStrLn "Each non-zero entry in input should be between 1 to 4."
    else if (not (count1 == 1 && count2 == 1 && count3 == 1 && count4 == 1))
    	then putStrLn "Each non-zero entry in input should occur exactly once."
    else do
        let solution = safetyHelper.solve $ sudokuBoard inListFinal
        printSudoku solution
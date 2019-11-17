import Data.List
import System.IO
import System.Random
import Data.Maybe
import System.IO.Unsafe

allTeams = ["BS","CM","CH","CV","CS","DS","EE","HU","MA","ME","PH","ST"]
newTeamList = []

getRandomIndex :: Int -> Int
getRandomIndex b = unsafePerformIO (getStdRandom (randomR (0, b)))

-- elemIndex :: Eq a => a -> [a] -> Maybe Int

removeItem :: String -> [String] -> [String]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = ys
                    | otherwise = y : removeItem x ys

generateNewTeamList :: Int -> [String] -> [String]
generateNewTeamList 0 _ = return [ ]
generateNewTeamList n allTeams = teamId : generateNewTeamList (n-1) y
    where
        teamId = allTeams !! randomIndex
        y = removeItem teamId allTeams
        randomIndex = getRandomIndex(n-1)

generateFixture :: Int -> [[Char]] -> IO ()
generateFixture 0 _ = return ()
generateFixture n newTeamList = 
    do
        let startIndex = (6-n)*2
        let team1 = newTeamList !! startIndex
        let team2 = newTeamList !! (startIndex+1)
        let date = ((6-n) `div` 2) + 1
        if (n-6) `rem` 2 == 0
            then putStrLn (team1 ++ " vs " ++ team2 ++ " " ++ (show date) ++ " " ++ " 9:30")
        else putStrLn (team1 ++ " vs " ++ team2 ++ " " ++ (show date) ++ " " ++ " 19:30")
        generateFixture (n-1) newTeamList

-- fixture :: [Char] -> IO ()
-- fixture "all" = generateFixture 6 $ init $ generateNewTeamList 12 allTeams
-- fixture team1 = 
-- 	do
-- 		teamIndex = elemIndex team1 newTeamList
-- 		if teamIndex `rem` 2 == 0
-- 			then team2 = newTeamList !! (teamIndex+1)
-- 		else team2 = newTeamList !! (teamIndex-1)
-- 		date = (teamIndex/4) + 1
-- 		teamIndex = teamIndex - (teamIndex/4)*4
-- 		if teamIndex/2 == 0
-- 			then time = " 9:30"
-- 		else time = " 19:30"
-- 		putStrLn team1 ++ " vs " ++ team2 ++ " " ++ (show date) ++ " " ++ time

-- nextMatch ::







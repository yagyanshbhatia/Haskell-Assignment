import Data.List
import System.IO
import System.Random
import Data.Maybe
import System.IO.Unsafe

allTeams = ["BS","CM","CH","CV","CS","DS","EE","HU","MA","ME","PH","ST"]


getRandomIndex :: Int -> Int
getRandomIndex b = unsafePerformIO (getStdRandom (randomR (0, b)))

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

-- take care of the case when allteams list is odd
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

newTeamList = init $ generateNewTeamList 12 allTeams

fixture :: [Char] -> IO ()
fixture "all" = generateFixture 6 newTeamList
fixture team1 = 
    do
        let teamIndex = fromJust $ elemIndex team1 newTeamList
        let time = if teamIndex `rem` 4 < 2
            then " 9:30"
            else " 19:30"
        let date = if teamIndex < 4
            then 1
            else if teamIndex < 8
                then 2
                else 3
        let team2 = if teamIndex `rem` 2 == 0
            then newTeamList !! (teamIndex+1)
            else newTeamList !! (teamIndex-1)
        putStrLn (team1 ++ " vs " ++ team2 ++ " " ++ (show date) ++ " " ++ time)

nextMatch :: Int -> Float -> IO()
nextMatch date time = 
    do
        let offset = if time < 9.31
            then 0
            else if time < 19.31
                then 1
                else 2
        let teamIndex = (date-1)*4 + 2*offset
        if teamIndex == 12
            then putStrLn "No matches ahead!"
            else fixture (newTeamList !! teamIndex)







import Data.List
import System.IO
import System.Random
import Data.Maybe

allTeams = ['BS','CM','CH','CV','CS','DS','EE','HU','MA','ME','PH','ST']
newTeamList = []

getRandomIndex :: Int -> Int
getRandomIndex b = randomRIO (0,b)

-- elemIndex :: Eq a => a -> [a] -> Maybe Int

removeItem :: String -> [String] -> [String]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

generateNewTeamList :: Int -> [String]
generateNewTeamList 0 = return newTeamList
generateNewTeamList n =
	do
		randomIndex = getRandomIndex(n-1)
		teamId = allTeams !! randomIndex
		newTeamList = newTeamList ++ teamId
		allTeams = removeItem teamId
		generateNewTeamList (n-1)

generateFixture :: Int
generateFixture 0 = return ()
generateFixture n = 
	do
		startIndex = (n-6)*2
		team1 = newTeamList !! startIndex
		team2 = newTeamList !! (startIndex+1)
		if (n-6) 'rem' 2 == 0
			then time = ' 9:30'
		else time = ' 19:30'
		date = (n-6)/2 + 1
		putStrLn team1 + ' vs ' + team2 + ' ' + date + ' ' + time
		generateFixture (n-1)

fixture :: String
fixture 'all' = generateFixture 6
fixture team1 = 
	do
		teamIndex = elemIndex team1 newTeamList
		if teamIndex 'rem' 2 == 0
			then team2 = newTeamList !! (teamIndex+1)
		else team2 = newTeamList !! (teamIndex-1)
		date = (teamIndex/4) + 1
		teamIndex = teamIndex - (teamIndex/4)*4
		if teamIndex/2 == 0
			then time = ' 9:30'
		else time = ' 19:30'
		putStrLn = team1 + ' vs ' + team2 + ' ' + date + ' ' + time

-- nextMatch ::
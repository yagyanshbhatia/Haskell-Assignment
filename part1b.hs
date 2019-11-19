import Data.Ord
import Data.List

-- Returns the tuple with maximum value
maxi xs = maximumBy (comparing fst) (zip xs [0..])

{-- map func x = map elements of parent list with the output of desired function;
	finds index of maximum element & return that element from list eventually
--}
greatest :: (a -> Int) -> [a] -> a
greatest func x = x!!ind where (_,ind) = maxi (map func x)
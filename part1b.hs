import Data.Ord
import Data.List

maxi xs = maximumBy (comparing fst) (zip xs [0..])

greatest :: (a -> Int) -> [a] -> a
greatest func x = x!!ind where (_,ind) = maxi (map func x)
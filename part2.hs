import Data.List
import Data.Function (on)


nonEmptySubstrings :: [a] -> [[a]]
nonEmptySubstrings = concatMap (tail . inits) . tails


anagrams x y = printAll $ nonEmptySubstrings (x++y)
compareFirst (x, _) (y, _) = x == y

-- 1. For each word, create a pair which consists of the sorted letters and the word itself.
-- 2. Sort the list of these pairs.
-- 3. Group the consecutive pairs which have the same sorted letters (i.e., the anagrams)
-- 4. Only consider the groups which have more than one such pair.
-- 5. Concatenate the groups with ++.
-- 6. Only take the second item of the pair, i.e., drop the sorted letters and just keep the word.

anaList words = map (map snd) $ filter ((>1).length) $ groupBy compareFirst $ sortBy (flip compare `on` fst) $ map (\w -> (sort w, w)) words
pairing a = [(x,y) | l<-a, (x:xs)<-tails l, y<-xs]

printAll x = mapM_ print $ nub $ pairing $ anaList x
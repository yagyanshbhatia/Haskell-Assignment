import Data.List 

-- Custom list defination
data List a = Empty | Cons a (List a) deriving Show

-- Recursive Function for converting into custom list from Haskell list
toList :: [a] -> List a
toList [] = Empty
toList x = Cons (head x) (toList (tail x))

-- Function to return elements from custom list
unwrapElem :: (List e) -> Maybe (e, List e)
unwrapElem Empty = Nothing
unwrapElem (Cons el rem) = Just (el,rem)

-- Recursive function for converting into Haskell list from custom list
toHaskellList :: List a -> [a]
toHaskellList Empty = []
toHaskellList x = unfoldr unwrapElem x
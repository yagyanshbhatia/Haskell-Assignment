import Data.List 

data List a = Empty | Cons a (List a) deriving Show

toList :: [a] -> List a
toList [] = Empty
toList x = Cons (head x) (toList (tail x))

unwrapElem :: (List e) -> Maybe (e, List e)
unwrapElem Empty = Nothing
unwrapElem (Cons el rem) = Just (el,rem)

toHaskellList :: List a -> [a]
toHaskellList Empty = []
toHaskellList x = unfoldr unwrapElem x
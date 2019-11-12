innerSum :: [Int] -> Int
innerSum x = sum x

innerMap :: [[Int]] -> [Int]
innerMap = map innerSum

prodVal :: [[Int]] -> Int
prodVal x = product (innerMap x)

main = do
    let x = [[1,2],[1,2]]
    print(prodVal x)
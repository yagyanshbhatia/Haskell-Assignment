innerSum :: [Int] -> Int
innerSum x = sum x

innerProd :: [[Int]] -> [Int]
innerProd = map innerSum

main = do
    let x = [[1,2],[1,2]]
    print(product (innerProd x))
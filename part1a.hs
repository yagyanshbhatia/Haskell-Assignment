innerSum :: [[Int]] -> [Int]
innerSum = map sum


innerProd :: [[Int]] -> Int
innerProd x = product (innerSum x)

-- Function to convert 2D list to 1D-internally summed list
innerSum :: [[Int]] -> [Int]
innerSum = map sum

-- Function to find product of summed-up lists
innerProd :: [[Int]] -> Int
innerProd [] = 0
innerProd x = product (innerSum x)

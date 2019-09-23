
-- Cantor Diagonal Example function
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method (must have n x n size to work for finite examples)
differentStream :: [[Int]] -> [Int]
differentStream ss = addComplementToEach (diagonalStream ss 0)

addComplementToEach :: [Int] -> [Int]
addComplementToEach [] = []
addComplementToEach xs | (head xs) == 1 = 0 : addComplementToEach (tail xs)
                       | (head xs) == 0 = 1 : addComplementToEach (tail xs)
                       | otherwise = 1 : addComplementToEach (tail xs)
                       

diagonalStream :: [[Int]] -> Int -> [Int]
diagonalStream [] n = [] 
diagonalStream ss n = [(head ss) !! n] ++ diagonalStream (tail ss) (n + 1)

testStream1 :: [[Int]]
testStream1 = [0..]:testStream1

testStream2 :: [[Int]]
testStream2 = repeatStream 0

repeatStream :: Int -> [[Int]]
repeatStream x = [x..]:(repeatStream (x+1))
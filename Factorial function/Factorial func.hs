import Data.List
import System.IO

allCom :: [Int]
allCom = [x | x <- [0..5], y <- [0..5], z <- [0..5]]

combo :: Int -> [Int]
combo 0 = []
combo n = n : combo (n - 1) 

newL :: Int -> [[Int]]
newL 0 = [[0]]
newL 1 = [[1]]
newL 2 = [[1,2],[2,1]]
newL n | n < 1 = error "Nice try Juan"
       | otherwise = buildUp n 2 (newL 2)

buildUp :: Int -> Int -> [[Int]] -> [[Int]]
buildUp n c xs | n == c = xs 
               | otherwise = buildUp n (c+1) (nextStep (c+1) xs) 

nextStep :: Int -> [[Int]] -> [[Int]]
nextStep n xs = addN n 0 (duplicate n xs)

addN :: Int -> Int -> [[Int]] -> [[Int]]
addN n p [] = []
addN n p xs = (putN n (p) (take (n-1) xs)) ++ (addN n (p+1) (drop (n-1) xs))

putN :: Int -> Int -> [[Int]] -> [[Int]]
putN n p [] = []
putN n p (x:xs) = ((take p x)++[n]++(drop p x)) : (putN n p xs) 

duplicate :: Int -> [a] -> [a]
duplicate 1 xs = xs 
duplicate n xs = xs ++ (duplicate (n-1) xs)

five :: Integer
five = 5
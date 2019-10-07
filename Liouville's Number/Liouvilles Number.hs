import Data.List
import System.IO

liou :: [Int]
liou = liou2 1 

liouExtra :: Int -> [Int] -> [Int]
liouExta 2 xs  = xs 
liouExtra n xs = liouExtra (n+1) (xs ++ (take (factorial (n + 1)) [0,0..]) ++ [1])

liou2 :: Int -> [Int]
liou2 1 = [1] ++ liou2 2
liou2 0 = []
liou2 n = (take ((factorial n) - (factorial (n - 1)) - 1) (repeat 0)) ++ [1] ++ liou2 (n + 1)

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)

largeFloat :: Float 
largeFloat = 0 + (10**(-5)) + (6*(10**(-11)))

liouv :: String
liouv = liouvF 1 

liouvF :: Int -> String
liouvF 1 = show(0.11) ++ (liouvF 3)
liouvF n = (take (((factorial (n - 1)) * (n - 1) ) - 1) (repeat '0')) ++ "1" ++ (liouvF (n + 1))

data Liouville = Liouville; 
instance Show Liouville where show _ = liouv;

hello_world = "hello world"

duplicate :: Int -> [a] -> [a]
duplicate 1 xs = xs 
duplicate n xs = xs ++ (duplicate (n-1) xs)

five :: Integer
five = 5

s :: String 
s = "lol"

n :: Int 
n = 4

um :: [Int]
um = [100,0,0,0]

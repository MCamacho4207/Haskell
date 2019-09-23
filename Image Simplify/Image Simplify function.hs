
--Image Simplify function
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)
simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList rs = simplifyRectangleList' rs rs 

simplifyRectangleList' :: [Rectangle] -> [Rectangle] -> [Rectangle]
simplifyRectangleList' [] ts = [(Rectangle (777,777) (777,777))] 
simplifyRectangleList' [r] ts = [r]
simplifyRectangleList' (r:rs) (t:ts) | coincidesInAll t ts = simplifyRectangleList' (removeItem t rs) (removeItem t rs) 
                                     | otherwise = simplifyRectangleList' (r:rs) (ts ++ [t])

areaRectangle :: Rectangle -> Int
areaRectangle (Rectangle (a,b) (c,d)) = (abs(a - c)) * (abs(b - c))

rectangleCoincidesCompletely :: Rectangle -> Rectangle -> Bool
rectangleCoincidesCompletely (Rectangle (s1,s2) (s3,s4)) (Rectangle (r1, r2) (r3,r4)) = (s1 >= r1) && (s1 <= r3) && (s2 >= r2) && (s2 <= r4) && (s3 <= r3) && (s3 >= r1) && (s4 <= r4) && (s4 >= r2)

coincidesInAll :: Rectangle -> [Rectangle] -> Bool
coincidesInAll r [] = False
coincidesInAll r rs = or (map (rectangleCoincidesCompletely r) rs)

rectangleIsAdjacent :: Rectangle -> Rectangle -> Bool
rectangleIsAdjacent r1 r2 = False 

removeItem :: Eq a => a -> [a] -> [a]
removeItem x [] = []
removeItem x xs | x == (head xs) = tail xs
                | otherwise = (head xs) : removeItem x (tail xs)

moveNext :: [Rectangle] -> [Rectangle]
moveNext (r:rs) = rs ++ [r]

tangleList = [(Rectangle (0,0) (2,1)), (Rectangle (0,0) (3,1)), (Rectangle (0,0) (5,1)), (Rectangle (0,0) (4,1))]

rec1 = (Rectangle (0,0) (5,1))
rec2 = (Rectangle (0,0) (4,2))

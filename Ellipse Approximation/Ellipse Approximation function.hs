
-- Ellipse Approximation function
-- convert an ellipse into a minimal list of rectangles representing its image
drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b = []

fullEclipseList :: [(Int, Int)] -> [Rectangle]
fullEclipseList [(x,y)] = [Rectangle (x,y) (x,y)]
fullEclipseList (x:xs) = (Rectangle x x) : fullEclipseList xs  

allContainedPoints :: Float -> Float -> Float -> Float -> [(Int, Int)]
allContainedPoints x y a b =[(v,w) | v <- (createCoordList x a), w <- (createCoordList y b), eclipseContruct (fromIntegral v) x (fromIntegral w) y a b <= 1]

createCoordList :: Float -> Float -> [Int]
createCoordList c l = [(floor c - (floor l) - 2)..(floor c + (floor l) + 2)]

eclipseContruct :: Float -> Float -> Float -> Float -> Float -> Float -> Float
eclipseContruct x xCentre y yCentre a b = ((x - xCentre)/a)^2 + ((y - yCentre)/b)^2

testEc :: Int -> Float -> Float
testEc i x = eclipseContruct (fromIntegral i) x (fromIntegral i) x x x 

h :: RealFrac a => a
h = 4
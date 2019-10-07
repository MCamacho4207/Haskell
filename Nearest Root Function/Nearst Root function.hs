
-- Nearest Root function
nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x x' eps = hillClimb (generatePolynomial' xs) x x' eps

hillFall :: (Float -> Float) -> Float -> Float -> Float -> Float
hillFall d x x' eps | (abs(x' - x)) <= sqrt(eps) = (x + x')/2
                    | d x1 <= d x2 = hillFall d x2 x' eps
                    | d x1 > d x2 = hillFall d x  x1 eps
                     where 
                        x1 = x  + ((sqrt(5) - 1)/2)*(x' - x)
                        x2 = x' - ((sqrt(5) - 1)/2)*(x' - x)

hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps | (abs(x' - x)) <= sqrt(eps) = (x + x')/2
                     | d x1 >= d x2 = hillClimb d x2 x' eps
                     | d x1 < d x2 = hillClimb d x  x1 eps
                     where 
                        x1 = x  + ((sqrt(5) - 1)/2)*(x' - x)
                        x2 = x' - ((sqrt(5) - 1)/2)*(x' - x)

generatePolynomial' :: [Float] -> (Float -> Float)
generatePolynomial' coeffs = \x -> (exPoly (squareFunc x (coeffs) ((length coeffs) - 1)) )

integrate :: [Float] -> Int -> (Float -> Float)
integrate [x] a = \y -> y*5
integrate coeffs maxOrder = \x -> x*( (last coeffs)/(fromIntegral maxOrder )*(x^(maxOrder - 1)) + times3 x ) 

expAscending :: Float -> [Float] -> Int -> Float
expAscending x coeffs 1 = (head coeffs) * x
expAscending x coeffs a = ( (head coeffs) * (1/fromIntegral a) * x^a ) + ( expAscending x (tail coeffs) (a-1) )

squareFunc :: Float -> [Float] -> Int -> Float
squareFunc x [y] a = y
squareFunc x xs a = (((last xs) * (x^a)) + squareFunc x (init xs) (a-1))

squareFunc' :: Float -> [Float] -> Float
squareFunc' x [y] = y
squareFunc' x xs = ((last xs) * (x^a)) + squareFunc' x (init xs) 
                  where a = (length xs) - 1

exPoly :: Float -> Float
exPoly x = (-1)*(x^2)

times3 :: Float -> Float
times3 x = 3 * x

floatHead :: [Float] -> Float
floatHead [] = 0
floatHead xs = head xs

floatTail :: [Float] -> [Float]
floatTail [] = [0]
floatTail [x] = []
floatTail (x:xs) = (head xs) : (floatTail xs)
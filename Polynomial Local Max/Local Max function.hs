
-- Local max function
-- search for the local maximum of f nearest x using an 
-- approximation margin delta and initial step value s
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps | (abs(x' - x)) <= sqrt(eps) = (x + x')/2
                     | d x1 >= d x2 = hillClimb d x2 x' eps
                     | d x1 < d x2 = hillClimb d x  x1 eps
                     where 
                        x1 = x  + ((sqrt(5) - 1)/2)*(x' - x)
                        x2 = x' - ((sqrt(5) - 1)/2)*(x' - x)

polynomial :: Float -> Float
polynomial x = x^2 +  5*x + 8
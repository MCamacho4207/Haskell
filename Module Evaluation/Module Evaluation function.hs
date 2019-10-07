
-- Module Result function
-- check whether the given results are suf#ficient to pass the year 
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)

canProgress :: [ModuleResult] -> Bool
canProgress ms = (creditSum ms 15) >= 60

classify :: [[ModuleResult]] -> DegreeClass
classify ms | length ms == 3 = classify3 ms
            | length ms == 4 = classify4 ms
            | otherwise = error "Not applicable Modules"



creditSum :: [ModuleResult] -> Float -> Float
creditSum [] cCredits = 0
creditSum (m:ms) cCredits | getModuleMark m >= 40 = (getModuleCredit m) + creditSum ms cCredits
                          | getModuleMark m >= 25 && cCredits > 0 = (compensationCredits m cCredits) + creditSum ms (cCredits - (compensationCredits m cCredits))
                          | otherwise = creditSum ms cCredits

compensationCredits :: ModuleResult -> Float -> Float
compensationCredits m limit | (getModuleCredit m) >= limit = limit
                            | otherwise = (getModuleCredit m)       

getModuleCredit :: ModuleResult -> Float
getModuleCredit (ModuleResult x y) = x

getModuleMark :: ModuleResult -> Int
getModuleMark (ModuleResult x y) = y

classify3 :: [[ModuleResult]] -> DegreeClass
classify3 ms | programmeMark < 50 = Third
             | programmeMark < 60 = LowerSecond 
             | programmeMark < 70 = UpperSecond
             | otherwise = First
              where programmeMark = ( (yearMark (ms !! 1)) + 2*(yearMark (ms !! 2)) )/3

classify4 :: [[ModuleResult]] -> DegreeClass
classify4 ms  | programmeMark < 50 = Third
              | programmeMark < 60 = LowerSecond 
              | programmeMark < 70 = UpperSecond
              | otherwise = First
               where programmeMark = ( (yearMark (ms !! 1)) + 2*(yearMark (ms !! 2)) + 2*(yearMark (ms !! 3)) )/4 

yearMark :: [ModuleResult] -> Float
yearMark [] = 0
yearMark ms = (realToFrac(yearIntMark ms)) / r
             where r = realToFrac (length ms)

yearIntMark :: [ModuleResult] -> Int
yearIntMark [] = 0
yearIntMark (m:ms) = (getModuleMark m) + (yearIntMark ms)

floatTest :: Float
floatTest = 8.99
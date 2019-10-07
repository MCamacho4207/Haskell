
-- Optimal Sequence function
data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)
executeInstructionSequence :: [Int] -> [Instruction] -> [Int]
executeInstructionSequence ns [] = ns
executeInstructionSequence ns [Pop] = drop 1 ns
executeInstructionSequence ns [Duplicate] = (head ns) : ns
executeInstructionSequence (n:ns) [Add] = (n + (head ns)) : (drop 1 ns) 
executeInstructionSequence (n:ns) [Multiply] = (n * (head ns)) : (drop 1 ns) 
executeInstructionSequence ns ins | (head ins) == Pop = executeInstructionSequence (executeInstructionSequence ns [Pop]) (drop 1 ins)
                                  | (head ins) == Duplicate = executeInstructionSequence (executeInstructionSequence ns [Duplicate]) (drop 1 ins)
                                  | (head ins) == Add = executeInstructionSequence (executeInstructionSequence ns [Add]) (drop 1 ins)
                                  | (head ins) == Multiply = executeInstructionSequence (executeInstructionSequence ns [Multiply]) (drop 1 ins)

optimalSequence :: Int -> [Instruction]
optimalSequence 1 = []
optimalSequence 2 = [Duplicate, Multiply]
optimalSequence 3 = [Duplicate, Duplicate, Multiply, Multiply]
optimalSequence n | length (generalFactors n) == 2 = [Duplicate] ++ optimalSequence (n-1) ++ [Multiply]
                  | otherwise = concat (map optimalSequence (primeFactors n))

generalFactors :: Int -> [Int]
generalFactors x = generalFactors' x x

generalFactors' :: Int -> Int -> [Int]
generalFactors' x 1 = [1]
generalFactors' x y | x `mod` y == 0 = y : (generalFactors' x (y - 1))
                    | otherwise = generalFactors' x (y - 1)

primeFactors :: Int -> [Int]
primeFactors x = primeFactors' x 2

primeFactors' :: Int -> Int -> [Int]
primeFactors' 1 x = [1]
primeFactors' x 1 = [x]
primeFactors' x y | x == y = [y]
                  | x `mod` y == 0 = y : primeFactors' (floor( (fromIntegral x) / fromIntegral(y) ) ) 2
                  | otherwise = primeFactors' x (y+1)
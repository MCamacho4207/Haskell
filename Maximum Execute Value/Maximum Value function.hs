
-- Maximum Value function
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

findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers [x] = []
findBusyBeavers xs | simpleExecute xs Pop > simpleExecute xs Add && simpleExecute xs Pop > simpleExecute xs Multiply = [Pop : concat (findBusyBeavers (exeInsSeq xs [Pop]))]
                   | simpleExecute xs Add > simpleExecute xs Pop && simpleExecute xs Add > simpleExecute xs Multiply = [Add : concat (findBusyBeavers (exeInsSeq xs [Add]))]
                   | simpleExecute xs Multiply > simpleExecute xs Pop && simpleExecute xs Multiply > simpleExecute xs Add = [Multiply : concat (findBusyBeavers (exeInsSeq xs [Multiply]))]
                   | simpleExecute xs Pop > simpleExecute xs Multiply && simpleExecute xs Pop == simpleExecute xs Add = [Pop : concat (findBusyBeavers (exeInsSeq xs [Pop]))] ++ [Add : concat (findBusyBeavers (exeInsSeq xs [Add]))]
                   | simpleExecute xs Pop > simpleExecute xs Add && simpleExecute xs Pop == simpleExecute xs Multiply = [Pop : concat (findBusyBeavers (exeInsSeq xs [Pop]))] ++ [Multiply : concat (findBusyBeavers (exeInsSeq xs [Multiply]))]
                   | simpleExecute xs Add > simpleExecute xs Pop && simpleExecute xs Add == simpleExecute xs Multiply = [Add : concat (findBusyBeavers (exeInsSeq xs [Add]))] ++ [Multiply : concat (findBusyBeavers (exeInsSeq xs [Multiply]))]
                   | simpleExecute xs Add == simpleExecute xs Pop && simpleExecute xs Add == simpleExecute xs Multiply = [Add : concat (findBusyBeavers (exeInsSeq xs [Add]))] ++ [Multiply : concat (findBusyBeavers (exeInsSeq xs [Multiply]))] ++ [Pop : concat (findBusyBeavers (exeInsSeq xs [Pop]))]
                   | otherwise = [[Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop]]

findBusyBeavers' :: [Int] -> [Instruction]
findBusyBeavers' [x] = []
findBusyBeavers' xs | simpleExecute xs Pop > simpleExecute xs Add && simpleExecute xs Pop > simpleExecute xs Multiply = Pop : findBusyBeavers' (exeInsSeq xs [Pop])
                    | simpleExecute xs Add > simpleExecute xs Pop && simpleExecute xs Add > simpleExecute xs Multiply = Add : findBusyBeavers' (exeInsSeq xs [Add])
                    | simpleExecute xs Multiply > simpleExecute xs Pop && simpleExecute xs Multiply > simpleExecute xs Add = Multiply : findBusyBeavers' (exeInsSeq xs [Multiply])
                    | simpleExecute xs Pop > simpleExecute xs Multiply && simpleExecute xs Pop == simpleExecute xs Add = Pop : findBusyBeavers' (exeInsSeq xs [Pop])
                    | simpleExecute xs Pop > simpleExecute xs Add && simpleExecute xs Pop == simpleExecute xs Multiply = Pop : findBusyBeavers' (exeInsSeq xs [Pop])
                    | simpleExecute xs Add > simpleExecute xs Pop && simpleExecute xs Add == simpleExecute xs Multiply = Add : findBusyBeavers' (exeInsSeq xs [Add])
                    | simpleExecute xs Add == simpleExecute xs Pop && simpleExecute xs Add == simpleExecute xs Multiply = Add : findBusyBeavers' (exeInsSeq xs [Add])
                    | otherwise = [Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop,Pop]

beaverFinder :: [Int] -> Int
beaverFinder [x] = 1
beaverFinder xs | simpleExecute xs Pop > simpleExecute xs Add && simpleExecute xs Pop > simpleExecute xs Multiply = beaverFinder (exeInsSeq xs [Pop])
                | simpleExecute xs Add > simpleExecute xs Pop && simpleExecute xs Add > simpleExecute xs Multiply = beaverFinder (exeInsSeq xs [Add])
                | simpleExecute xs Multiply > simpleExecute xs Pop && simpleExecute xs Multiply > simpleExecute xs Add = beaverFinder (exeInsSeq xs [Multiply])
                | simpleExecute xs Pop > simpleExecute xs Multiply && simpleExecute xs Pop == simpleExecute xs Add = 2 * beaverFinder (exeInsSeq xs [Pop])
                | simpleExecute xs Pop > simpleExecute xs Add && simpleExecute xs Pop == simpleExecute xs Multiply = 2 * beaverFinder (exeInsSeq xs [Add])
                | simpleExecute xs Add > simpleExecute xs Pop && simpleExecute xs Add == simpleExecute xs Multiply = 2 * beaverFinder (exeInsSeq xs [Multiply])
                | simpleExecute xs Add == simpleExecute   xs Pop && simpleExecute xs Add == simpleExecute xs Multiply = 3 * beaverFinder (exeInsSeq xs [Pop])
                | otherwise = 777777777

simpleExecute :: [Int] -> Instruction -> Int
simpleExecute xs ins = head (executeInstructionSequence xs [ins])

exeInsSeq :: [Int] -> [Instruction] -> [Int]
exeInsSeq xs ins = executeInstructionSequence xs ins

addElementToSuperList :: a -> [[a]] -> [[a]]
addElementToSuperList x [] = [[x]]
addElementToSuperList x (xs:xss) = ((x:xs):xss)
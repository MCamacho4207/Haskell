
-- List number order function
-- split a given list into sub-lists 
-- each of these must be strictly ascending, descending, or equal
splitSort :: Ord a => [a] -> [[a]] 
splitSort xs = dropLastElement (splitSortPrimary xs)

splitSortPrimary :: Ord a => [a] -> [[a]] 
splitSortPrimary [] = [[]]
splitSortPrimary [x] = [[x],[]]
splitSortPrimary (n:ns) | n < head ns = [n : increasingList ns] ++ splitSortPrimary incNS
                        | n > head ns = [n : decreasingList ns] ++ splitSortPrimary decNS
                        | n == head ns = [n : equalList ns] ++ splitSortPrimary eqNS
                        where incNS = drop (length (increasingList (n:ns))) (n:ns)
                              decNS = drop (length (decreasingList (n:ns))) (n:ns)
                              eqNS  = drop (length (equalList (n:ns))) (n:ns) 

increasingList :: Ord a => [a] -> [a]
increasingList [x] = [x]
increasingList (x:xs) | x < head xs = x : increasingList xs
                      | otherwise = x : []

decreasingList :: Ord a => [a] -> [a]
decreasingList [x] = [x]
decreasingList (x:xs) | x > head xs = x : decreasingList xs
                      | otherwise = x : []

equalList :: Ord a => [a] -> [a]
equalList [x] = [x]
equalList (x:xs) | x == head xs = x : equalList xs
                 | otherwise = x : []

dropLastElement :: [a] -> [a]
dropLastElement xs = take ((length xs) - 1) xs






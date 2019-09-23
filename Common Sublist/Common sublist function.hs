
-- Common sub-list function
-- longest common sub-list of a finite list of finite list
longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList [] = []
longestCommonSubList xss = chooseLowestLength (commonSub xss)

candidates :: Eq a => [[a]] -> [a]
candidates xss = [x | xs <- xss, x <- xs, (existsAll x xss)]

existsAll :: Eq a => a -> [[a]] -> Bool
existsAll x [] = True
existsAll x (xs:xss) = (existsElement x xs) && (existsAll x xss)

existsElement :: Eq a => a -> [a] -> Bool
existsElement x xs = or (map (==x) xs)

isSubList :: Eq a => [a] -> [a] -> Bool
isSubList subxs xs = False

commonSub :: Eq a => [[a]] -> [[a]]
commonSub (xs:xss) = [subListOf xs ys | ys <- xss]

chooseLowestLength :: Eq a => [[a]] -> [a]
chooseLowestLength xss = chooseLowestLength' xss (head xss)

chooseLowestLength' :: Eq a => [[a]] -> [a] -> [a]
chooseLowestLength' [] ys = ys
chooseLowestLength' (xs:xss) ys | (length xs) < (length ys) = chooseLowestLength' xss xs 
                                | otherwise = chooseLowestLength' xss ys

subListOf :: Eq a => [a] -> [a] -> [a]
subListOf xs ys = subListOf' xs ys xs ys

subListOf' :: Eq a => [a] -> [a] -> [a] -> [a] -> [a]
subListOf' xs ys [] [] = []
subListOf' xs ys xs' [] = subListOf' xs ys (tail xs') ys
subListOf' xs ys [] ys' = []
subListOf' xs ys (x:xs') (y:ys') | x == y = x : subListOf' (removeAllBefore x xs) (removeAllBefore y ys) (removeAllBefore x xs) (removeAllBefore y ys)
                                 | xs' == [] = []
                                 | otherwise = subListOf' xs ys (x:xs') ys' 

iterateUntil :: Eq a => [a] -> Int
iterateUntil [] = error ("Empty List")
iterateUntil [x] = 0
iterateUntil (x:xs) | x == (head xs) = iterateUntil [x]
                    | otherwise = 1 + iterateUntil xs

removeItem :: Eq a => a -> [a] -> [a]
removeItem x [] = []
removeItem x xs | x == (head xs) = tail xs
                | otherwise = (head xs) : removeItem x (tail xs)

removeAllBefore :: Eq a => a -> [a] -> [a]
removeAllBefore x [] = []
removeAllBefore x (y:ys) | x == y = ys
                         | otherwise = removeAllBefore x ys 



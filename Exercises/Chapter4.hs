--safe version of some list functions
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:[]) = Just []
safeInit x = Just (init x)

-------------------------------------------------------------------------------
--function that takes a predicate and a list of any type, and splits its input list on every element for which the predicate returns False

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith pred (x:xs)
  | pred x == False = splitWith pred xs
  | otherwise = [x] : (splitWith pred xs)

--splitWith (elem 2) [[2,3],[2,3],[4]]
-------------------------------------------------------------------------------
--program that prints the first word of each line of its input











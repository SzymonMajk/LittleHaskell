--function that computes the number of elements in a list
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

--myLength [1..15]
--myLength []
-------------------------------------------------------------------------------
--function that computes the mean of a list
myMean :: (Fractional a, Integral b) => [b] -> a
myMean [] = error "empty list"
myMean x = (fromIntegral (sum x)) / (fromIntegral (myLength x))

--myMean []
--myMean [5,17,52]
-------------------------------------------------------------------------------
--function that turn list into list palindrom
myPalindrome :: [a] -> [a]
myPalindrome [] = []
myPalindrome x = x ++ (reverse x)

--myPalindrome "abc"
-------------------------------------------------------------------------------
--function that derermines whether its input is palindrom
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) = if x == (last xs) then isPalindrome (init xs)
                      else False

--isPalindrome "kajak"
--isPalindrome "123321"
--isPalindrome $ myPalindrome "123"
-------------------------------------------------------------------------------
--function that sorts a list of lists based on the length of each sublist
sortDependsOnLength :: [[a]] -> [[a]]
sortDependsOnLength [] = []
sortDependsOnLength (x:xs) = sortDependsOnLength ([y | y <- xs, length(y) < length (x)]) ++ [x] ++ sortDependsOnLength ([y | y <- xs, length(y) > length (x)])

--sortDependsOnLength ["abc","","abcd","ad"]
-------------------------------------------------------------------------------
--function that joins a list of lists together using a separator value
intersperse :: a -> [[a]] -> [a]
intersperse sep [] = []
intersperse sep [x] = x
intersperse sep (x:xs) = x ++ [sep] ++ (intersperse sep xs)

--intersperse ',' ["I","have","something","to","tell."]
-------------------------------------------------------------------------------
--funtion that deretimen height of binary tree
data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

treeHeigh :: (Num a, Ord a) => Tree t -> Int
treeHeigh Empty = 0
treeHeigh (Node _ left right) = 1 + max (treeHeigh left) (treeHeigh right)

--treeHeigh Empty
--treeHeigh (Node 5 (Node 4 Empty (Node 2 Empty (Node 4 Empty Empty))) Empty)
-------------------------------------------------------------------------------
--function 





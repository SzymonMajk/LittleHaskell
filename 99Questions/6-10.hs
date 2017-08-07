-------------------------------------------------------------------------------
------------------------------------------------------------------------------- -- Problem 6 - find out whether a list is a palindrome

myPalindrome :: (Eq a) => [a] -> Bool
myPalindrome [] = error "Empty list"
myPalindrome [_] = True
myPalindrome list = foldr (&&) True (checkEquality list (reverse list)) where
  checkEquality [] [] = []
  checkEquality (x:xs) (y:ys) = if x == y then True:(checkEquality xs ys)
                                else False:(checkEquality xs ys)
--clever palindrome xs = foldr (&&) True $ zipWith (==) xs (reverse xs)
--myPalindrome [1,2,3]
--myPalindrome "madamimadam"
--myPalindrome [1,2,4,8,16,8,4,2,1]
-------------------------------------------------------------------------------
-- Problem 7 - flatten a nested list structure
-- We have to define a new data type, because lists in Haskell are homogeneous
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List nestedList) = foldr (++) [] (map myFlatten nestedList)

--myFlatten (Elem 5)
--myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
--myFlatten (List [])
-------------------------------------------------------------------------------
-- Problem 8 - eliminate consecutive duplicates of list elements, the order of the elements should not be changed

myCompress :: (Eq a) => [a] -> [a]
myCompress list = foldr compressAcc [] list where
  compressAcc x [] = [x]
  compressAcc x acc 
    | x == head acc = acc
    | otherwise = x : acc

--myCompress "aaaabccaadeeee"
-------------------------------------------------------------------------------
-- Problem 9 - pack consecutive duplicates of list elements into sublists if a list contains repeated elements they should be placed in separate sublists

myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack [x] = [[x]]
myPack (x:xs) = if x `elem` (head (myPack xs)) 
  then (x:(head (myPack xs))):(tail (myPack xs)) 
  else [x]:(myPack xs)

--myPack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a','a', 'd', 'e', 'e', 'e', 'e']
-------------------------------------------------------------------------------
-- Problem 10 - consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E

myEncode :: (Eq a) => [a] -> [(Int,a)]
myEncode list = [ (length l, head l) | l <- myPack list]

--myEncode "aaaabccaadeeee"
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

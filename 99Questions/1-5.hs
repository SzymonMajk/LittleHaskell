-------------------------------------------------------------------------------
------------------------------------------------------------------------------- -- Problem 1 - last element of list, prelude also provides functon last

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- myLast [2,3,4]
-- myLast "abcd"
-------------------------------------------------------------------------------
-- Problem 2 - last but one element of list

myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [x] = error "To few elements"
myButLast (x:_:[]) = x
myButLast (_:_:xs) = myButLast xs

-- myButLast [1,2,3,4]
-- myButLast ['a'..'z']
-------------------------------------------------------------------------------
-- Problem 3 - k'th element of a list, first is number 1

myElementAt :: (Num k, Ord k) => [a] -> k -> a
myElementAt (x:xs) k
   | k == 1 = x
   | k > 1 = myElementAt xs (k-1)
   | otherwise = error "Wrong Index"

-- myElementAt [1,2,3] 2
-- myElementAt haskell 5
-------------------------------------------------------------------------------
-- Problem 4 - number of elements of a list

--- myLengthClever = fst . last . zip [1..] ---
myLength :: [a] -> Int
myLength xs = myLengthAcc xs 0
   where 
        myLengthAcc [] k = k
        myLengthAcc (x:xs) k = myLengthAcc xs (k + 1)

-- myLength [123, 456, 789]
-- myLength "Hello, world!"
-------------------------------------------------------------------------------
-- Problem 5 - reverse a list

myReverse :: [a] -> [a]
myReverse list = myReverseAcc list []
   where
        myReverseAcc [] acc = acc
        myReverseAcc (x:xs) acc = myReverseAcc xs (x:acc)

-- myReverse "A man, a plan, a canal, panama!"
-- myReverse "kajak"
-- myReverse [1,2,3,4]
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

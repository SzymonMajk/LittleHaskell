import Data.Char(digitToInt)

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

fWord :: [Char] -> [Char]
fWord [] = []
fWord (x:xs)
 | x == ' ' || x == '\t' = ' ':[]
 | otherwise = x:(fWord xs)

fWords :: [[Char]] -> [Char]
fWords [] = []
fWords (x:xs) = (fWord x) ++ (fWords xs)

printfWords :: String -> IO ()
printfWords linesIn = putStrLn (fWords (lines linesIn))

--printfWords "I want to say something\nlike everyone know\nlambda style is easy and pleasant"
-------------------------------------------------------------------------------
--use a fold write asInt function, that takes String represents Int and return its value

--digitValue index digit = 10 * index + (digitToInt digidt)

transformDigit index digit = 10 * index + (digitToInt digit)

asInt acc ('-':xs) = -foldl transformDigit acc xs
asInt acc xs = foldl transformDigit acc xs

--asInt 0 "53535"
--asInt 0 "-42445345543543543534"
-------------------------------------------------------------------------------
--function that concat list of list into single list using foldl


concat :: [[a]] -> [a]
concat xs = foldr (++) [] xs

--Prelude.concat ["One"," and "," another..."]
-------------------------------------------------------------------------------
--write recursive and fold takeWhile funtions

recTakeWhile :: (a->Bool) -> [a] -> [a]
recTakeWhile _ [] = []
recTakeWhile p (x:xs) = if p x then x:(recTakeWhile p xs) else []

foldTakeWhile :: (a->Bool) -> [a] -> [a]
foldTakeWhile p xs = foldr step [] xs where
                   step y ys = if p y then y:ys else []

--recTakeWhile (<3) [1,2,3,4,5]
--foldTakeWhile (<3) [1,2,3,4,5]
-------------------------------------------------------------------------------
--any, cycle with fold

foldAny :: (a->Bool) -> [a] -> Bool
foldAny p xs = foldr step False xs where
             step y ys = p y || False

--foldAny (==1) [1,2,3]
--foldAny (==1) [2,3,4]

foldCycle :: [a] -> [a]
foldCycle xs = foldr (:) (foldCycle xs) xs

--take 10 (foldCycle [1,2,3])

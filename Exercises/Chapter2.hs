--function that returns the element before the last
lastButOne :: [a] -> a
lastButOne [] = error "List is empty"
lastButOne [_] = error "List have only one element"
lastButOne (x:y:[]) = x
lastButOne (x:y:z) = lastButOne (y:z)

--lastButOne []
--lastButOne [2]
--lastButOne [2,4]
--lastButOne [3,5,6]
--lastButOne ['A'..'Z']

module P01 where

-- p01
last' :: [a] -> a
last' [] = error "empty!"
last' [x] = x
last' (_: xs) = last' xs
--last' (x: xs)
--  | null xs = x
--  | otherwise = last' xs

-- p02
lastButOne :: [a] -> a
lastButOne [x] = error "empty!"
lastButOne [x, l] = x
lastButOne (x: xs) = lastButOne xs

-- p03
elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty!"
elementAt (x: _) 1 = x
elementAt (_: xs) n = elementAt xs $ n -1

-- p04
length' :: [a] -> Int
length' = foldl (\acc _ -> acc +1) 0

-- p05
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x: xs) = reverse' xs ++ [x]

-- p06
isPalin :: (Eq a) => [a] -> Bool
isPalin [x] = True
isPalin (x:xs) = (x == last xs) && (isPalin . init $ xs)

-- p07 REDO LATER
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x: xs)) = flatten x ++ flatten (List xs)

-- p08
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (x:y:xs)
  | x == y = compress $ x : xs
  | otherwise = x : compress (y : xs)

-- p09
pack :: (Eq a) => [a] -> [[a]]
pack = foldl (flip addOn) []

addOn :: (Eq a) => a -> [[a]] -> [[a]]
addOn x [] = [[x]]
addOn x acc =
  let ctx = last acc
  in if x `elem` ctx
      then init acc ++ [x : ctx]
      else acc ++ [[x]]

-- p10
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\ps -> (length ps, head ps)) . pack
module P11 where

import P01

-- p11
data EncodeInfo a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [EncodeInfo a]
encodeModified = map encMod . encode

encMod :: (Int, a) -> EncodeInfo a
encMod (cnt, x)
  | cnt == 1 = Single x
  | otherwise = Multiple cnt x

-- p12
decodeModified :: [EncodeInfo Char] -> String
decodeModified = foldl (\acc a -> acc ++ decodeToString a ) []

decodeToString :: EncodeInfo Char -> String
decodeToString (Single a) = [a]
decodeToString (Multiple n a) = foldl (\acc _ -> a : acc) []  [1..n]

-- p13
encodeDirect :: String -> [EncodeInfo Char]
encodeDirect (x: xs) = encodeLength x 0 xs

encodeLength :: Char -> Int -> String -> [EncodeInfo Char]
encodeLength a 0 [] = [Single a]
encodeLength a n [] = [Multiple (n + 1) a]
encodeLength a n (x:xs)
  | a == x = encodeLength a (n + 1) xs
  | otherwise = encodeLength a n [] ++ encodeLength x 0 xs

-- p14
dupli :: [a] -> [a]
dupli = foldl (\acc x -> acc ++ [x,x]) []

-- p15
repli :: [a] -> Int -> [a]
repli xs n = foldl (\acc x -> acc ++ map (\_ -> x) [1..n]) [] xs

-- p16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n 1

dropEvery' :: [a] -> Int -> Int -> [a]
dropEvery' [] _ _ = []
dropEvery' (x:xs) n ctr
  | ctr == n = dropEvery' xs n 1
  | otherwise = x : dropEvery' xs n (ctr +1)

-- p17
split :: [a] -> Int -> ([a], [a])
split xs n = split' n ([], xs)

split' :: Int -> ([a], [a]) -> ([a], [a])
split' _ (x, []) = (x, [])
split' 0 (x, xs) = (x, xs)
split' n (x, (xh:xt)) = split' (n-1) (x ++ [xh], xt)

-- p18
slice :: [a] -> Int -> Int -> [a]
slice x start end = drop (start -1) $ take end x


-- p19
rotate :: [a] -> Int -> [a]
rotate a 0 = a
rotate a n
  | n > 0 = rotate ((tail a) ++ [head a]) (n-1)
  | otherwise = rotate (last a : init a) (n+1)

-- p20
removeAt :: Int -> [a] ->  ([a], [a])
removeAt n (x:xs)= removeAt' n ([], x, xs)

removeAt' :: Int -> ([a], a, [a]) -> ([a], [a])
removeAt' _ (xi, xl, []) = ([], xi ++ [xl])
removeAt' 1 (xi, a, xl) = ([a], xi ++ xl)
removeAt' n (h, a, (x:xs)) = removeAt' (n-1) (h ++ [a], x, xs)
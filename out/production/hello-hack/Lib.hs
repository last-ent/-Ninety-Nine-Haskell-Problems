{-# LANGUAGE BlockArguments #-}
module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

inRange :: Int -> String
inRange x
  | x `elem` [1 .. 5] = "Hello!"
  | otherwise = show x ++ " Not here!"

sort :: [Int] -> [Int] -> [Int]
sort al bl
  | null al = bl
  | null bl = al
  | a < b = a : sort as bl
  | otherwise = b : sort al bs
  where
    (a: as) = al
    (b: bs) = bl

maximum' :: (Ord a) => [a] -> a
maximum' xs
  | null xs = error "blah!"
  | null xt = xh
  | xh > maxTail = xh
  | otherwise = maxTail
  where
    (xh: xt) = xs
    maxTail = maximum' xt
    
maximum'' :: (Ord a) => [a] -> a
maximum'' xs@(x: xt)
  | null xs = error "blah!!"
  | null xt = x
  | otherwise = max x (maximum'' xt)
  
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n xs
  | n <= 0 = []
  | null xs = []
  | otherwise = head xs : take' (n-1) (tail xs)
  
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xt) = reverse' xt ++ [x]

quickSort :: (Ord a) => [a] -> [a]
quickSort []  = []
quickSort (x:xs) = 
  let smallerSorted = quickSort [a | a <- xs, a <= x]
      biggerSorted = quickSort [ a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted
  
zip' :: [a] -> [a] -> [(a, a)]
zip' _ [] = []
zip' [] _ = []
zip' (x: xs) (y: ys) = (x, y) : zip' xs ys


data ABC = ABC  { x :: Int}

instance Show ABC where
  show (ABC x) = "Show => " ++ show x
  
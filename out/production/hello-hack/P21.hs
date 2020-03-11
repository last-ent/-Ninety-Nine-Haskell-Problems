module P21 where

-- p21
insertAt :: [a] -> [a] -> Int -> [a]
insertAt ins a n = []

insertAt' :: Int -> [a] -> ([a], [a]) -> [a]
insertAt' n ins (hd, tl@(th:tt))
  | n == 1 = hd ++ ins ++ tl 
  | otherwise = insertAt' (n-1) ins (hd ++ [th], tt) 
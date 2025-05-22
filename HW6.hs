--  Assignment
--  Omar Gonzalez
--  AKVTE9
--  #############################################################

-- 1. 'a' characters in a text
countAChars :: String -> Int
countAChars [] = 0
countAChars  (x:xs)
    | x == 'a' = 1 + countAChars xs
    | otherwise = countAChars xs

-- 2. Lucas-series
lucas :: Integer -> Integer
lucas 0 = 2
lucas 1 = 1
lucas x = lucas (x-1) + lucas (x-2)

-- 3. Useful function in the future
longerThan :: [a] -> Integer -> Bool
longerThan _ n 
    | n < 0 = True
longerThan [] _ = False
longerThan (_:xs) n = longerThan xs (n - 1)
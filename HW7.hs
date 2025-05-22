--  Assignment
--  Omar Gonzalez
--  AKVTE9
--  #############################################################

import Data.List
-- 1. Is it sorted?
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [a] = True
isSorted (x:y:xs) 
    | y >= x = True && isSorted(y:xs)
    | otherwise = False
-- 2. "Better" indexing
(!!!) :: Integral b => [a] -> b -> a
(!!!) (x:xs) n
    | n == 0 = x
    | n > 0 = (!!!) xs (n-1)
    | otherwise = (!!!) (reverse (x:xs)) (abs n - 1)
-- 3. Formatting
format :: Integral i => i -> String -> String
format len str
    | len <= 0 = str
    | null str = ' ' : format (len - 1) ""
    | otherwise = head str : format (len - 1) (tail str)
-- 4. Too many spaces
deleteDuplicateSpaces :: String -> String
deleteDuplicateSpaces str = unwords $ words str
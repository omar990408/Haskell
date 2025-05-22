--  Assignment
--  Omar Gonzalez
--  AKVTE9
--  #############################################################
import Data.Char
-- 1. Recursion
alternatingCase :: String -> Bool
alternatingCase [ ] = True
alternatingCase [x] = isUpper x
alternatingCase (x:y:xs) = isUpper x && isLower y && alternatingCase xs
-- 2. Infix deletion
deleteInfixes :: Eq a => [a] -> [a] -> [a]
deleteInfixes [] ls2 = ls2
deleteInfixes _ [] = []
deleteInfixes ls1 ls2 = helper ls2
    where
        x = length ls1
        helper [] = []
        helper ls2
            | ls1 == take x (ls2) = helper (drop x ls2)
            | otherwise = head ls2 : helper (tail ls2)
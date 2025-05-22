--  Assignment
--  Omar Gonzalez
--  AKVTE9
--  #############################################################
import Data.Char


-- 1. Character conversion
toUpperEveryThird :: String -> String
toUpperEveryThird = counter 1
    where
        counter :: Int -> String -> String
        counter _ [] = []
        counter i (x:xs)
            | i `mod` 3 == 0 = toUpper x : counter (i+1) xs
            | otherwise = x : counter (i+1) xs
-- 2. Double those elements
doubleElements :: [a] -> [a]
doubleElements [] = []
doubleElements (x:xs) = x:x:doubleElements(xs)

-- 3. Stormy region
mightyGale :: (Num a, Ord b, Num b, Integral c) => [(String, a, b, c)] -> String
mightyGale [] = ""
mightyGale ((name, _, windSpeed, _):rest)
  | windSpeed > 110 = name
  | otherwise      = mightyGale rest
--   4. Keeper of secrets
cipher :: String -> String
cipher [] = []
cipher [_] = ""
cipher [_, _] = ""
cipher str@(x:y:z:xs)
    | isDigit z = [x,y]
    | otherwise = cipher (y:z:xs)
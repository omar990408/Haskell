module SAMPLE where

import Data.Char
import Data.List
import Data.Maybe

-- Indices of empty lists

indicesOfEmpties :: Eq a => [[a]] -> [Int]
indicesOfEmpties ls =  helper 1 (map null ls)
    where 
        helper :: Int -> [Bool] -> [Int]
        helper _ [] = []
        helper n (x:xs)
            | x = n : helper (n+1) xs
            | otherwise = helper (n+1) xs

-- indicesOfEmpties :: Eq a => [[a]] -> [Int]
-- indicesOfEmpties ls = helper 1 [length list | list <-ls]
--     where 
--         helper :: Int -> [Int] -> [Int]
--         helper _ [] = []
--         helper n (x:xs)
--             | x == 0 = n : helper (n+1) xs
--             | otherwise = helper (n+1) xs

-- Modification of words
applyOnWords :: (String -> String) -> String -> String
applyOnWords func str = unwords $ map func $ words $ str

-- Replace all occurrences
replaceAll :: Eq a => a -> [a] -> a -> [a]
replaceAll _ [] _ = []
replaceAll toF ls@(x:xs) toR
    | toF == x = toR : replaceAll toF xs toR
    | otherwise = x : replaceAll toF xs toR

-- Shape

data Shape = Circle Double 
            | Rectangle Double Double 
            | Triangle Double Double 
            deriving (Eq,Show)


scale :: Double -> Shape -> Shape
scale x (Circle a) = Circle (x*a)
scale x (Rectangle a b) = Rectangle (x*a) (x*b)
scale x (Triangle a b) = Triangle (x*a) (x*b)


-- Conditional transformation
applyWhile :: (a -> Bool) -> (a -> a) -> [a] -> [a]
applyWhile _ _ [] = []
-- applyWhile pred func ls = [func i| i <- ls, pred i]
applyWhile pred func ls@(x:xs)
    | pred x = func x : applyWhile pred func xs
    | otherwise = x : xs


-- -- Fixed point search
-- fixedPointIn :: Eq a => (a -> a) -> a -> Int -> Maybe Int

-- Character set
lackOfLetters :: String -> [Char] -> Maybe [Char]
lackOfLetters str font
    | null missing = Nothing
    | otherwise = Just missing
    where
        textTransformed = nub $ map toLower str
        missing = textTransformed \\ font
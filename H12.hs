--  Assignment
--  Omar Gonzalez
--  AKVTE9
--  #############################################################
-- Mandatory tasks
-- 1. Replace
import Data.List

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace old new xs
  | isPrefixOf old xs = new ++ replace old new (drop (length old) xs)
  | otherwise = head xs : replace old new (tail xs)

-- 2. While
-- while :: (a -> Bool) -> (a -> a) -> a -> a
-- while p f x
--   | p x = while p f (f x)
--   | otherwise = x
while p f x = go x x
  where
    go prev x'
      | p x'     = go x' (f x')
      | otherwise = prev

-- 3. Pascal-triangle
pascal :: [[Integer]]
pascal = [1] : map (\row -> 1 : zipWith (+) row (tail row) ++ [1]) pascal
import Data.List
--  Assignment
--  Omar Gonzalez
--  AKVTE9
--  #############################################################
-- Mandatory tasks
-- 1. Pairwise
pairwise :: (a -> a -> b) -> [a] -> [b]
pairwise pr ls = zipWith pr ls $ drop 1 ls
-- 2. Indexed map
indexedMap :: Integral index => (index -> a -> b) -> [a] -> [b]
indexedMap pr ls = zipWith pr (map fromIntegral [0..]) ls
-- 3. Windowed
windowedMap :: ([a] -> b) -> Int -> [a] -> [b]
windowedMap pr n ls
    | n <= 0 = map (const (pr [])) ls
    | otherwise = map pr $ filter (\ys -> length ys == n) $ map (take n) $ tails ls
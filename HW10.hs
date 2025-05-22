--  Assignment
--  Omar Gonzalez
--  AKVTE9
--  #############################################################
-- Mandatory tasks
-- 1. Pairwise
pairwise :: (a -> a -> b) -> [a] -> [b]
pairwise p [] = []
pairwise p [x] = []
pairwise p (x:y:xs) = p x y : pairwise p (y:xs)
-- 2. Lot of application on lots of lists
apsOnLists :: [a -> b] -> [[a]] -> [[b]]
apsOnLists p@(px:pxs) ls@(lsx:lsxs) = map px lsx : apsOnLists pxs lsxs
apsOnLists _ _ = []

mapping :: [(Char, Char)]
mapping = zip chars (drop 3 (cycle chars))
  where
    chars = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']


encodeCaesar :: String -> String
encodeCaesar = map (\c -> maybe '?' id (lookup c mapping))

decodeCaesar :: String -> String
decodeCaesar = map (\c -> maybe '?' id (lookup c reverseMapping))
    where reverseMapping = [(b, a) | (a, b) <- mapping]



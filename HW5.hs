--  Assignment
--  Omar Gonzalez
--  AKVTE9
--  #############################################################

import Data.List

-- 1. Compression of a text
compress :: (Eq a) => [a] -> [(a, Int)]
compress ls = [(head xs,length xs) | xs <- group ls  ]

-- 2. Decompression of a text
decompress :: [(a,Int)] -> [a]
decompress ls = concat [replicate (snd x) (fst x)| x <- ls]
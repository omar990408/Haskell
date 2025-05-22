--  Assignment
--  Omar Gonzalez
--  AKVTE9
--  #############################################################

-- 1. Hours and Minutes
-- Generate a list that contains all (hour, minute) pairs in a day.

hoursAndMinutes :: [(Integer, Integer)]
hoursAndMinutes = [(h,m) | h <- [0..23] ,m <-[0..59]]

-- 2. Incrementing
-- Define the add1 function which adds 1 to every number of a list.

add1 :: [Integer] -> [Integer]
add1 [] = []
add1 (x:xs) = [n+1|n <- (x:xs)]

-- 3. Lists with a single element
-- Define the onlySingletons function which keeps the singleton lists from a list of lists. Use list comprehension in the solution!

singleton :: [a] -> Bool
singleton [x] = True
singleton _ = False

onlySingletons :: [[a]] -> [[a]]
onlySingletons ls = [x |x <- ls, singleton (x)]


-- 4. Dominoes
-- Generate a list of all “dominoes” with the given number of dots : [(0,0),(0,1),...,(0,n),(1,1),...,(n,n)].
-- The dominoes in this exercise should contain at least zero, at most n dots (i.e. they are numbered from (0,0) to (n,n)).
-- Note: Repetitions should be avoided. For example: domino (1,0) shall not be in the list because it is already in it as (0,1).

dominoes :: Integer -> [(Integer, Integer)]
dominoes n = [(x,y) | x <- [0..n], y <- [0..n], y>=x ]



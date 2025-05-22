--  Assignment
--  Omar Gonzalez
--  AKVTE9
--  #############################################################

--  1. Exactly three
isTripleton :: [a] -> Bool
isTripleton (x:y:z:[]) = True
isTripleton _ = False

-- 2. Two or at least four
exactly2OrAtLeast4 :: [a] -> Bool
exactly2OrAtLeast4 [] = False
exactly2OrAtLeast4 [x] = False
exactly2OrAtLeast4 (x:y:z:[]) = False
exactly2OrAtLeast4 _ = True

--3. Starts with zero
startsWithZero :: [Integer] -> Bool
startsWithZero (0:xs) = True
startsWithZero (_:xs) = False
startsWithZero [] = False

-- 4. First two
firstTwoElements :: [a] -> [a]
firstTwoElements (x:y:xs) = (x:y:[])
firstTwoElements [] = []
firstTwoElements [_] = []

-- 5. Without the fourth
withoutFourth :: [a] -> [a]
withoutFourth (x:y:z:w:xs) = (x:y:z:xs)
withoutFourth (x:xs) = x:xs
withoutFourth [] = []
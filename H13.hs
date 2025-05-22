--  Assignment
--  Omar Gonzalez
--  AKVTE9
--  #############################################################
-- Mandatory tasks
-- a) Time type
-- Define the Time type! The type should have a single constructor called T which should have two Ints as parameters, 
-- which are representing the hours and minutes. Ask the compiler to instantiate the Eq class for our Time type!
-- (There cannot be anything after the Eq!)

-- type Hours = Int
-- type Mins = Int

data Time = T Int Int deriving (Eq)

-- b) Smart constructor
t :: Int -> Int -> Time
t h m 
    | (h >= 0 && h <=23) && (m >=0 && m<= 59) = T h m
    | otherwise = error "Time values out of range"

-- c) It's show time!

instance Show Time where
    show (T h m) = show h ++ ":" ++ show m

-- d) Earlier? Later?
-- Instantiate the Ord class for the Time type to order the timestamps correctly.

instance Ord Time where
    compare (T h1 m1) (T h2 m2)
        | h1 < h2 = LT
        | h1 > h2 = GT
        | m1 < m2 = LT
        | m1 > m2 = GT
        | otherwise = EQ

-- e) In between
isBetween :: Time -> Time -> Time -> Bool
isBetween t1 t2 t3 = (t1 <= t2 && t2 <= t3) || (t3 <= t2 && t2 <= t1)

-- f) American time

data USTime = AM Int Int | PM Int Int deriving Eq

-- g) Smart constructors of USTime

ustimeAM :: Int -> Int -> USTime
ustimeAM h m
    | (h >= 1 && h <= 12) && (m >= 0 && m <= 59) = AM h m
    | otherwise =  error "Time values out of range"

ustimePM :: Int -> Int -> USTime
ustimePM h m
    | (h >= 1 && h <= 12) && (m >= 0 && m <= 59) = PM h m
    | otherwise =  error "Time values out of range"

-- h) Time to show US!

instance Show USTime where
    show (AM h m) = "AM " ++ format h m
    show (PM h m) = "PM " ++ format h m

format :: Int -> Int -> String
format h m = show h ++ ":" ++ pad m

pad :: Int -> String
pad m
    | m < 10    = '0' : show m
    | otherwise = show m

-- i) American orders

instance Ord USTime where
    compare (AM 12 m1) (AM 12 m2) = compare (0, m1) (0, m2)
    compare (AM 12 m1) (AM h2 m2) = compare (0, m1) (h2, m2)
    compare (AM h1 m1) (AM 12 m2) = compare (h1, m1) (0, m2)
    compare (AM h1 m1) (AM h2 m2) = compare (h1, m1) (h2, m2)

    compare (PM 12 m1) (PM 12 m2) = compare (12, m1) (12, m2)
    compare (PM 12 m1) (PM h2 m2) = compare (12, m1) (h2 + 12, m2)
    compare (PM h1 m1) (PM 12 m2) = compare (h1 + 12, m1) (12, m2)
    compare (PM h1 m1) (PM h2 m2) = compare (h1 + 12, m1) (h2 + 12, m2)

    compare (AM 12 m1) (PM 12 m2) = compare (0, m1) (12, m2)
    compare (AM 12 m1) (PM h2 m2) = compare (0, m1) (h2 + 12, m2)
    compare (AM h1 m1) (PM 12 m2) = compare (h1, m1) (12, m2)
    compare (AM h1 m1) (PM h2 m2) = compare (h1, m1) (h2 + 12, m2)

    compare (PM 12 m1) (AM 12 m2) = compare (12, m1) (0, m2)
    compare (PM 12 m1) (AM h2 m2) = compare (12, m1) (h2, m2)
    compare (PM h1 m1) (AM 12 m2) = compare (h1 + 12, m1) (0, m2)
    compare (PM h1 m1) (AM h2 m2) = compare (h1 + 12, m1) (h2, m2)

-- j) To 24-hour format
ustimeToTime :: USTime -> Time
ustimeToTime (AM 12 m) = t 0 m
ustimeToTime (AM h m)  = t h m
ustimeToTime (PM 12 m) = t 12 m
ustimeToTime (PM h m)  = t (h + 12) m

-- k) To 12-hour format
timeToUSTime :: Time -> USTime
timeToUSTime (T 0 m)  = ustimeAM 12 m
timeToUSTime (T h m)
    | h < 12          = ustimeAM h m
    | h == 12         = ustimePM 12 m
    | otherwise       = ustimePM (h - 12) m

-- 2. Div the head   

divHead :: Integral a => [a] -> a -> Maybe a
divHead [] _ = Nothing
divHead _ 0 = Nothing
divHead (x:_) n = Just (x `div` n)

-- 3. Nothings are not needed

dropMaybes :: [Maybe a] -> [a]
dropMaybes [] = []
dropMaybes (Nothing:xs) = dropMaybes xs
dropMaybes (Just y:xs) = y : dropMaybes xs

-- 4. Index of an element
elemIndex' :: (Eq a, Num b) => a -> [a] -> Maybe b
elemIndex' val xs = helper 0 xs
  where
    helper _ [] = Nothing
    helper i (y:ys)
      | val == y  = Just i
      | otherwise = helper (i + 1) ys
--  1st Assignment
--  Omar Gonzalez
--  AKVTE9
--  #############################################################

--  1. Vector addition and subtraction

addV :: (Double,Double) -> (Double,Double) -> (Double,Double)
addV (x1,y1) (x2,y2) = (x1+x2,y1+y2)

subV :: (Double,Double) -> (Double,Double) -> (Double,Double)
subV (x1,y1) (x2,y2) = (x1-x2,y1-y2)

-- 2. Multiplying by a scalar

scaleV :: Double -> (Double,Double) -> (Double,Double)
scaleV s (x,y) = (s*x,s*y)

-- 3. Scalar multiplication

scalar :: (Double,Double) -> (Double,Double) -> Double
scalar (x1,y1) (x2,y2) = (x1*x2) + (y1*y2)
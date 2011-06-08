import Data.List 

data Direction = RightTurn | LeftTurn| Straight
		 deriving (Show,Eq)
data Point = Point{
		x :: Double,
		y :: Double
		}
	     deriving (Show,Eq)	

crossProduct :: Point -> Point -> Point -> Double
crossProduct (Point x1 y1) (Point x2 y2) (Point x3 y3) = ((x2-x1)*(y3-y1) - (y2-y1)*(x3-x1))

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt((x1-x2)^2 + (y1-y2)^2) 
		
turn :: Point -> Point -> Point -> Direction  
turn a b c = let checkTurn x | x == 0 = Straight
			     | x < 0 = RightTurn
			     | x > 0 = LeftTurn
	     in checkTurn (crossProduct a b c)

turns :: [Point] -> [((Point,Point,Point),Direction)]
turns points | ((length points) < 3) = []
turns (point1:point2:point3:points) = ((point1,point2,point3),turn point1 point2 point3) :  (turns (point2:point3:points))

compareYPoint (Point x1 y1) (Point x2 y2) | y1 == y2 = compare x1 x2
					  | y1 < y2  = LT
					  | y1 > y2  = GT

compareCross pvt a b = let angle = compare (crossProduct pvt a b) 0
			   nearer =  compare (distance pvt a) (distance pvt b)
		       in if (angle == EQ) then nearer else angle

pointsFromTupleList :: [(Double,Double)] -> [Point]
pointsFromTupleList = map (\(x,y) -> Point x y)

lowestY :: [Point] -> Point
lowestY = minimumBy compareYPoint

grahamScan :: [Point] -> [Point]
grahamScan points = map fst (filter (\(x,d) -> d /= RightTurn) (turns sortedPoints))  where
      p = nub points
      pvt = lowestY p
      sortedPoints = pvt : (sortBy (compareCross pvt) (delete pvt p)) ++ [pvt,pvt]




  	


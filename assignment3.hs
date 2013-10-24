data State= State {
	cars 	:: [Car],
	hVal	:: Int
} deriving (Show)

data Car = Car {
	coords	:: [Point],
	ident	:: Char
} deriving (Show)

data Point = Point {
	x 		:: Int,
	y 		:: Int
} deriving (Show)

-- Process Input Signal - Eg. What Kurt wants to put in
buildCarList :: [String] -> [Car] -> State
buildCarList stringList carList  
	| null stringList 				= State carList 0
	| otherwise						= buildCarList (tail stringList) (processLine (head stringList) (length stringList) carList) 

processLine :: String -> Int ->  [Car] -> [Car]
processLine line yCoord carList  
	| null line						= carList
	| otherwise 					= processLine (tail line) yCoord ( processChar (head line) (length line) yCoord carList )
	
processChar :: Char -> Int -> Int -> [Car] -> [Car]
processChar a xCoord yCoord carList 
	| null carList && a /= '-'		= [ ( Car [ (Point xCoord yCoord) ] a ) ]
	| null carList 					= carList
	| a == ( ident (head carList) )	= (Car ((Point xCoord yCoord) : ( coords (head carList) ) ) a) : tail carList
	| otherwise 					= head carList : processChar a xCoord yCoord (tail carList)

-- Build next states for each car in the list of cars
generateNewStates :: [Car] -> State -> [State] -> [State]
generateNewStates carList currentState newStates
	| null carList 					= newStates
	| otherwise						= generateNewStates (tail carList) currentState (merge newStates ( statesForCar (head carList) currentState [] ) )

statesForCar :: Car -> State -> [State]
statesForCar car currentState = merge (moveForward car currentState) (moveBackward car currentState)

moveForward :: Car -> State -> [State]
moveForward car currentState = [ State (nextCar : otherCars) (hVal currentState) ]
	where	nextCar = 
			otherCars = remove (ident car) (carList currentState)

	| orientation == vertical		= moveLeft (ident car) currentState
	| orientation == horizontal		= moveUp   (ident car) currentState
	where orientation = (getOrientation (coords car))

moveBackward :: Car -> State -> [State]
moveBackward car currentState = [ State (nextCar : otherCars) (hVal currentState) ]
	where	nextCar = 
			otherCars = remove (ident car) (carList currentState)
	--| orientation == vertical		= moveRight (ident car) currentState
	--| orientation == horizontal		= moveDown  (ident car) currentState
	--where orientation = (getOrientation (coords car))

--moveLeft :: Char -> State -> [State]
--moveLeft i currentState
--	| canMoveLeft i currentState 			= updateState i currentState left
--	| otherwise 							= []

--moveRight :: Char -> State -> [State]
--moveRight i currentState
--	| canMoveRight i currentState 			= updateState i currentState right
--	| otherwise 							= []

--moveUp :: Char -> State -> [State]
--moveUp i currentState
--	| canMoveUp i currentState 				= updateState i currentState up
--	| otherwise 							= []

--moveDown :: Char -> State -> [State]
--moveDown i currentState
--	| canMoveDown i currentState 			= updateState i currentState down
--	| otherwise 							= []

-- Order of the cars is irrelevant in the list. 
updateState :: String -> State -> Int -> [State]
updateState i currentState direction 		= [nextState]
	where nextState = State ( updatedCarList (hVal currentState)
		  updatedCarList = (updateCar i (getCar i (cars currentState))) : removeCar i (cars currentState))
-- make sure the list stays in oder ( smallest value point at the head )
updateCar :: Char -> Car -> Int -> Car
updateCar i car direction
	| direction == left 					= ( Car ( points ++ (nextPoint ) ) i )
	| direction == right 					= ( Car ( ( pointToRight point) : points ) i )
	| direction == up 						= ( Car ( points ++ ( pointAbove point ) ) i )
	| direction == down 					= ( Car ( ( pointBelow point )  : points ) i )
	where 	point = head (coords car)
			points = tail (coords car)

getCar :: Char -> [Car] -> Car
getCar i carList
	| null carList 						= []
	| ( ident (head carList) ) == i 	= head carList
	| otherwise 						= getCar i (tail carList)

-- Merge Helper Function
merge :: [a] -> [a] -> [a]
merge xs [] 							= xs
merge [] ys  							= ys
merge (x:xs) (y:ys) 					= x : y : merge xs ys

-- helper method for the Car structure
getOrientation :: [Point] -> Int
getOrientation points
	| (x head points) == (x head (tail points ) ) = horizontal
	| (y head points) == (y head (tail points ) ) = vertical
	| otherwise 								  = -1

-- helper methods for the point structure
nextPoint :: Point -> Int -> Int -> Point
nextPoint p1 orientation j
	| orientation == vertical 			= Point (x p1) ( (y p1) + j )
	| otherwise 						= Point ( (x p1) + j ) (y p1)

canMove :: Car -> State -> Int -> Int -> Bool
canMove p1 currentState orientation j 
	| orientation == vertical 			= nextPoint p1 orientation j
	| otherwise

--pointToLeft :: Point -> Point
--pointToLeft  p1 = (Point (xP1 + 1) yP1)
--	where 	xP1 = x (p1)
--			yP2 = y (p1)

--pointToRight :: Point -> Point
--pointToRight p1 = (Point (xP1 - 1) yP1)
--	where 	xP1 = x (head (coords car))
--			yP2 = y (head (coords car))

--pointAbove :: Point -> Point
--pointAbove 	p1 = (Point xP1 (yP1 + 1))
--	where 	xP1 = x (head (coords car))
--			yP2 = y (head (coords car))

--pointBelow :: Point -> Point
--pointBelow 	p1 = (Point xP1 (yP1 - 1))
--	where 	xP1 = x (head (coords car))
--			yP2 = y (head (coords car))

vertical 	= 0
horizontal 	= 1

right 		= 0
up 			= 1
left 		= 2
down 		= 3
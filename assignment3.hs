data State= State {
	cars 	:: [Car],
	hVal	:: Int
} deriving (Show, Eq)

data Car = Car {
	coords	:: [Point],
	ident	:: Char
} deriving (Show, Eq)

data Point = Point {
	x 		:: Int,
	y 		:: Int
} deriving (Show, Eq)

rushHour :: [String] -> [State]
rushHour input						= statesearch [firstState] []
	where firstState = buildFirstState input []

statesearch :: [State] -> [State] -> [State]
statesearch unexplored path
   | null unexplored              	= []
   | (isGoalState currentState)		= currentState : path
   | ( not (null result) )         	= result
   | otherwise                    	= statesearch (init unexplored) path
     where 
     	result 			= statesearch nextStates (currentState : path)
        currentState 	= (last unexplored)
        nextStates 		= prune (generateStates currentState) path

getNthState :: State -> Int -> [State]
getNthState currentState n 	
	| n == 0 						= [currentState]
	| (isGoalState currentState) 	= [currentState]
	| otherwise 					= currentState : getNthState (last (generateStates currentState)) (n-1)

bestState :: [State] -> State
bestState stateList 	= getState (maximum (map hVal stateList)) stateList

generateStates :: State -> [State]
generateStates currentState = generateNewStates currentState (cars currentState)

generateNewStates :: State -> [Car] -> [State]
generateNewStates currentState carList
	| null carList 					= []
	| otherwise 					= merge newStates recursionTerm
	where
		newStates 		= (statesForCar currentState (head carList) ) 
		recursionTerm	= (generateNewStates currentState (tail carList))

statesForCar :: State -> Car -> [State]
statesForCar currentState car  		= merge moveForward moveBackward
	where
		moveForward = (move car forward currentState)
		moveBackward= (move car backward currentState)

move :: Car -> Int -> State -> [State]
move car modifier currentState
	| canMove nextPt otherCars 		= [ State (nextCar : otherCars) (getHVal currentState) ]
	| otherwise  					= []
	where	
		nextCar 	= updateCar nextPt (coords car) modifier (ident car)
		otherCars	= removeCar (ident car) (cars currentState)
		nextPt 		= nextPoint car modifier

canMove :: Point -> [Car] -> Bool
canMove p1 carList 	
	| null carList					= True
	| collision p1 (head carList) 	= False
	| outOfBounds p1 				= False
	| otherwise 					= canMove p1 (tail carList)

-- make sure the list stays in oder ( smallest value point at the head )
updateCar :: Point -> [Point] -> Int -> Char -> Car
updateCar point points modifier identifier
	| modifier == forward 			= ( Car ( point : ( tail points ) ) identifier )
	| modifier == backward			= ( Car ( ( tail points ) ++ [point] ) identifier )

nextPoint :: Car -> Int -> Point
nextPoint car modifier
	| modifier == backward 			= processNextPoint (last (coords car)) (getOrientation car) modifier
	| modifier == forward 			= processNextPoint (head (coords car)) (getOrientation car) modifier

processNextPoint :: Point -> Int -> Int -> Point
processNextPoint p1 orientation modifier
	| orientation == vertical 		= Point (x p1) ( (y p1) + modifier )
	| otherwise 					= Point ( (x p1) + modifier ) (y p1)

	
collision :: Point -> Car -> Bool
collision p1 car 					= elem p1 (coords car)

outOfBounds :: Point -> Bool
outOfBounds p1 						= ( (x p1) > 6 ) || ( (x p1) < 1 ) || ( (y p1) > 6 ) || ( (y p1) < 1 )

--
-- Heuristic 
-- Distance of the XX Car to the goal state -5 per tile away
-- Number of Points occupied in the same row as X car -10 per other occupant
-- This system will bias the moves towards removing occupants over moving 
-- forwards and once the path is clear always move forward. 
--
getHVal :: State -> Int
getHVal state 							= distanceToGoal (getCar 'X' (cars state) )  + stuffInTheWay (cars state) 0

distanceToGoal :: Car -> Int
distanceToGoal car 						= (5) * ( 6 - ( x (last (coords car) ) ) )

stuffInTheWay :: [ Car ] -> Int -> Int
stuffInTheWay carList returnVal
	| null carList 						= returnVal
	| inRow (coords (head carList))		= stuffInTheWay (tail carList) (returnVal - 10)
	| otherwise 						= stuffInTheWay (tail carList) (returnVal)
	
inRow :: [Point] -> Bool
inRow points							
	| x (head points) == 3 				= True
	| otherwise 						= False

--
-- Helper Methods
--
getCar :: Char -> [Car] -> Car
getCar i carList
	| ( ident (head carList) ) == i 	= head carList
	| otherwise 						= getCar i (tail carList)

getState :: Int -> [State] -> State
getState i stateList
	| ( hVal (head stateList) ) == i 	= (head stateList)
	| otherwise 						= getState i (tail stateList)

removeCar :: Char -> [Car] -> [Car]
removeCar i carList
	| null carList 						= []
	| i == ( ident (head carList) )		= (tail carList)
	| otherwise 						= (head carList) : (removeCar i (tail carList))

getOrientation :: Car -> Int
getOrientation car
	| (x (head points)) == (x (last points) ) 	= vertical
	| (y (head points)) == (y (last points) ) 	= horizontal
	| otherwise 								= -1
	where points = (coords car)

isGoalState :: State -> Bool
isGoalState currentState 				= winningPosition (getCar 'X' (cars currentState) )

winningPosition :: Car -> Bool
winningPosition car 					= elem (Point 1 4) (coords car)

prune :: [State] -> [State] -> [State]
prune xs []								= xs
prune [] ys								= ys
prune (x:xs) ys
	| elem x ys 						= prune xs ys
	| otherwise 						= prune xs (x:ys) 

merge :: [a] -> [a] -> [a]
merge xs [] 							= xs
merge [] ys  							= ys
merge (x:xs) (y:ys) 					= x : y : merge xs ys

-- "Constants" (I know they're not really constants)
horizontal 	= 0
vertical	= 1

forward 	= 1
backward 	= (-1)

-- Process Input Signal - Eg. What Kurt wants to put in
buildFirstState :: [String] -> [Car] -> State
buildFirstState stringList carList  
	| null stringList 				= State carList 0
	| otherwise						= buildFirstState (tail stringList) (processLine (head stringList) (length stringList) carList) 

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

test1 = ["--B---",	"--B---",	"XXB---",	"--AA--",	"------",	"------" ]
test0 = ["------", "------", "XX----", "------", "------", "------" ]
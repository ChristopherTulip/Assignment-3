-- Christopher Tulip
-- 24047094
--
-- Attempted Heuristic. I hope it works well.
-- See commented section labeled Heuristic for more details
--

import Data.List

data State= State {
	cars 	:: [Car],
	hVal	:: Int
} deriving (Show, Eq, Ord)

data Car = Car {
	coords:: [Point],
	ident	:: Char
} deriving (Show, Eq, Ord)

data Point = Point {
	x 		:: Int,
	y 		:: Int
} deriving (Show, Eq, Ord)

rushHour :: [String] -> [[String]]
rushHour input						= printPath (myStateSearch firstState [] [])
	where firstState = buildFirstState input []

myStateSearch :: State -> [State] -> [State] -> [State]
myStateSearch currentState path attempted
	|	(isGoalState currentState)			= currentState : path
	| ((null path) && (not (null attempted))) = attempted -- attempted == full state space
	| null nextStates 								= myStateSearch (head path) (tail path) ( currentState : attempted )
	| elem next attempted 						= myStateSearch (head path) (tail path) ( currentState : attempted )
	| elem best attempted 						= myStateSearch next (currentState : path) (currentState : attempted )
	| otherwise 											= myStateSearch best (currentState : path) (currentState : attempted)
	where
		nextStates  = (sortBy sortState (generateStates currentState))
		best 				= (head nextStates)
		next 				= (getUntriedState nextStates attempted)

getUntriedState :: [State] -> [State] -> State
getUntriedState nextStates attempted
	| null (tail nextStates)						= head nextStates
	| isGoalState (head nextStates)			= head nextStates
	| elem (head nextStates) attempted 	= getUntriedState (tail nextStates) attempted
	| otherwise 												= (head nextStates)


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
statesForCar currentState car  		= merge moveBackward moveForward
	where
		moveForward = (move car forward currentState)
		moveBackward= (move car backward currentState)

move :: Car -> Int -> State -> [State]
move car modifier currentState
	| canMove nextPt otherCars 		= [ State (sortBy sortCars (nextCar : otherCars)) (getHVal currentState) ]
	| otherwise  									= []
	where
		nextCar 	= updateCar nextPt (coords car) modifier (ident car)
		otherCars	= removeCar (ident car) (cars currentState)
		nextPt 		= nextPoint car modifier

canMove :: Point -> [Car] -> Bool
canMove p1 carList
	| outOfBounds p1 							= False
	| null carList								= True
	| collision p1 (head carList) = False
	| otherwise 									= canMove p1 (tail carList)

-- make sure the list stays in oder ( smallest value point at the head )
updateCar :: Point -> [Point] -> Int -> Char -> Car
updateCar point points modifier identifier
	| modifier == backward 			= ( Car ( point : ( init points ) ) identifier )
	| modifier == forward				= ( Car ( ( tail points ) ++ [point] ) identifier )

nextPoint :: Car -> Int -> Point
nextPoint car modifier
	| modifier == backward 			= processNextPoint (head (coords car)) (getOrientation car) modifier
	| modifier == forward 			= processNextPoint (last (coords car)) (getOrientation car) modifier

processNextPoint :: Point -> Int -> Int -> Point
processNextPoint p1 orientation modifier
	| orientation == vertical 	= Point (x p1) ( (y p1) + modifier )
	| otherwise 								= Point ( (x p1) + modifier ) (y p1)


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
getHVal state
	|	(isGoalState state) = 10000000000
	| otherwise 					= 200 + distanceToGoal (getCar 'X' (cars state) ) + stuffInTheWay (cars state) 0

distanceToGoal :: Car -> Int
distanceToGoal car 			= (50) * ( 6 - ( x (last (coords car) ) ) )

stuffInTheWay :: [ Car ] -> Int -> Int
stuffInTheWay carList returnVal
	| null carList 										= returnVal
	| inRow (coords (head carList))		= stuffInTheWay (tail carList) (returnVal - 10)
	| otherwise 											= stuffInTheWay (tail carList) (returnVal)

inRow :: [Point] -> Bool
inRow points
	| x (head points) == 3 				= True
	| otherwise 									= False

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
prune newStates oldStates
	| null oldStates									= newStates
	| elem (head oldStates) newStates = prune (tail newStates) (tail oldStates)
	| otherwise 											= prune newStates (tail oldStates)

merge :: [a] -> [a] -> [a]
merge xs [] 							= xs
merge [] ys  							= ys
merge (x:xs) (y:ys) 			= x : y : merge xs ys

sortCars car1 car2
	| (ident car1) > (ident car2)	= GT
	| otherwise 						= LT

sortState state1 state2
	| (hVal state1) > (hVal state2)	= GT
	| otherwise 						= LT

getNthStates :: State -> Int ->[State]
getNthStates currentState n
	| n == 0 							= [currentState]
	| n < 2 							= (generateStates currentState)
	| (isGoalState currentState) 	= [currentState]
	| otherwise 					= currentState : getNthStates (head (generateStates currentState)) (n-1)

getNthState :: State -> Int -> [State]
getNthState currentState n
	| n == 0 							= [currentState]
	| (isGoalState currentState) 	= [currentState]
	| otherwise 					= currentState : getNthState (head (generateStates currentState)) (n-1)

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

-- process output signal
printPath :: [State] -> [[String]]
printPath path -- = map printState path
	| null path 						= []
	| otherwise 						= printState (head path) : printPath (tail path)

printState :: State -> [String]
printState state = printCars (cars state) blankList

printCars :: [Car] -> [String] -> [String]
printCars carList returnString
	| null carList 					= returnString
	| otherwise 						= printCars (tail carList) (putCarInString (head carList) returnString)

putCarInString :: Car -> [String] -> [String]
putCarInString car returnString = putPointsInString (coords car) (ident car) returnString

putPointsInString :: [Point] -> Char -> [String] -> [String]
putPointsInString points i returnString
	| null points 					= returnString
	| otherwise 						= putPointsInString (tail points) i (putPointInString (head points) i returnString)

putPointInString :: Point -> Char -> [String] -> [String]
putPointInString point i returnString = getStringRow (y point) (x point) i returnString

getStringRow :: Int -> Int -> Char -> [String] -> [String]
getStringRow y x i returnString
	| y == 6 								= replaceChar x i (head returnString) : tail returnString
	| otherwise 						= head returnString : getStringRow (y + 1) x i (tail returnString)

replaceChar :: Int -> Char -> String -> String
replaceChar x i returnString
	| x == 6 								= i : tail returnString
	| otherwise 						= (head returnString) : replaceChar (x + 1) i (tail returnString)

test3 = ["--GG--",	"--B---",	"XXB---",	"--B---",	"--EE--",	"------" ]
test2 = ["--B---",	"--B---",	"XXB---",	"--AA--",	"----EE",	"------" ]
test1 = ["--B---",	"--B---",	"XXB---",	"--AA--",	"------",	"------" ]
test0 = ["------", "------", "XX----", "------", "------", "------" ]
blankList = ["------", "------", "------", "------", "------", "------" ]

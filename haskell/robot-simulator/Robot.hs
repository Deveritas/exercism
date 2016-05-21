module Robot (Bearing(..), Robot, mkRobot, coordinates, simulate, bearing, turnRight, turnLeft) where

data Bearing = North | East | South | West deriving (Enum, Eq, Show)
type Coordinates = (Int, Int)

data Robot = Robot {
    bearing :: Bearing,
    coordinates :: Coordinates
} deriving (Eq, Show)

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft b = pred b

turnRight :: Bearing -> Bearing
turnRight West = North
turnRight b = succ b

mkRobot :: Bearing -> Coordinates -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate = foldl (\robot step -> case step of 'L' -> robot {bearing = turnLeft (bearing robot)}
                                              'R' -> robot {bearing = turnRight (bearing robot)}
                                              'A' -> robot {coordinates = move (bearing robot) (coordinates robot)}
                                              _ -> error "unknown instruction")

move :: Bearing -> Coordinates -> Coordinates
move North (x, y) = (x, y+1)
move South (x, y) = (x, y-1)
move East (x, y) = (x+1, y)
move West (x, y) = (x-1, y)
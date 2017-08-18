{-
Author: Zubin Kadva, 902772316, zkadva2016@my.fit.edu
Course: CSE 5400, Fall 2016
Project: Robot
Example run:
*Main> destination [Forward 20, Backward 10, TurnRight, Forward 100]
(100,10)
-}

import Prelude hiding (Right, Left)

data Command 
    = Forward Int
    | Backward Int
    | TurnLeft
    | TurnRight

-- Compute commands starting from 0, 0 and facing upwards
destination :: [Command] -> (Int, Int)
destination list = position
  where (position, _) = foldl go ((0, 0), Up) list

-- Store all directions in a datatype for easy comparison
data Facing 
    = Up
    | Right
    | Down
    | Left
    deriving (Enum)

-- Turn left
turnLeft :: Facing -> Facing
turnLeft direction = pred direction

-- Turn right
turnRight :: Facing -> Facing
turnRight direction = succ direction

-- Travel in a given direction with distance
go :: ((Int, Int), Facing) -> Command -> ((Int, Int), Facing)
go (xy, direction) TurnLeft = (xy, turnLeft direction)
go (xy, direction) TurnRight = (xy, turnRight direction)
go ((x, y), direction) (Forward distance) =
  case direction of
    Up     -> ((x, y+distance), Up)
    Down   -> ((x, y-distance), Down)
    Left   -> ((x-distance, y), Left)
    Right  -> ((x+distance, y), Right)    
go ((x, y), direction) (Backward distance) =
  case direction of
    Up     -> ((x, y-distance), Up)
    Down   -> ((x, y+distance), Down)
    Left   -> ((x+distance, y), Left)
    Right  -> ((x-distance, y), Right)      
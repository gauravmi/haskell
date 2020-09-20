data Direction = N | E | W | S deriving (Show, Eq, Read)

leftD :: Direction -> Direction
leftD d
  | d == N = W
  | d == W = S
  | d == S = E
  | d == E = N

rightD :: Direction -> Direction
rightD d
  | d == N = E
  | d == E = S
  | d == S = W
  | d == W = N

data Position = Position {x :: Int, y::Int} deriving (Show, Read)
data Rover = Rover { position :: Position, direction :: Direction} deriving (Show)

left :: Rover -> Rover
right :: Rover -> Rover
move :: Rover -> Rover

left r = Rover (Position (x (position r)) (y (position r))) (leftD (direction r))
right r = Rover (Position (x (position r)) (y (position r))) (rightD (direction r))

move (Rover (Position x y) direction)
  | direction == N = Rover (Position x (y+1)) direction
  | direction == E = Rover (Position (x+1) y) direction
  | direction == W = Rover (Position (x-1) y) direction
  | direction == S = Rover (Position x (y-1)) direction

type OriginX = Int
type OriginY = Int
rover :: OriginX -> OriginY -> Direction -> Rover
rover x y direction = Rover (Position x y) direction

run :: Rover -> String -> Rover

run r [] = r
run rover (x:xs)
  | x == 'M' = run (move rover) xs
  | x == 'L' = run (left rover) xs
  | x == 'R' = run (right rover) xs

main = do
  [ inputX, inputY, inputD, pattern ] <- sequence [getLine, getLine, getLine, getLine]
  let x = read inputX :: Int
  let y = read inputY :: Int
  let d = read inputD :: Direction
  let rover  = Rover (Position x y) d
  print $ run rover pattern


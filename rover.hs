data Direction = N | E | W | S deriving (Show, Eq, Read)

data State = State {
  leftD:: Direction,
  rightD:: Direction
} deriving (Show, Read, Eq)

state N = State W E
state W = State S N
state S = State E W
state E = State N S

data Position = Position {x :: Int, y::Int} deriving (Show, Read)
data Rover = Rover { position :: Position, direction :: Direction} deriving (Show)

left :: Rover -> Rover
left (Rover position direction) = Rover position (leftD $ state direction)

right :: Rover -> Rover
right (Rover position direction) = Rover position (rightD $ state direction)

move :: Rover -> Rover
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
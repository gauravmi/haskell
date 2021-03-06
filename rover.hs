data Direction = N | E | W | S deriving (Show, Eq, Read)

data State = State {
  left:: Direction,
  right:: Direction
} deriving (Show, Read, Eq)

state N = State W E
state W = State S N
state S = State E W
state E = State N S

data Position = Position {x :: Int, y::Int} deriving (Show, Read)

front, back, top, bottom :: Position -> Position
front (Position x y)  = Position (x+1) y
back (Position x y)   = Position (x-1) y
top (Position x y)    = Position x (y+1)
bottom (Position x y) = Position x (y-1)

data Rover = Rover { position :: Position, direction :: Direction} deriving (Show)

turnLeft :: Rover -> Rover
turnLeft (Rover position direction) = Rover position (left $ state direction)

turnRight :: Rover -> Rover
turnRight (Rover position direction) = Rover position (right $ state direction)

move :: Rover -> Rover
move (Rover p@(Position x y) N) = Rover (top p) N
move (Rover p@(Position x y) E) = Rover (front p) E
move (Rover p@(Position x y) W) = Rover (back p) W
move (Rover p@(Position x y) S) = Rover (bottom p) S

type OriginX = Int
type OriginY = Int
rover :: OriginX -> OriginY -> Direction -> Rover
rover x y direction = Rover (Position x y) direction

run :: Rover -> String -> Rover
run r [] = r
run rover (x:xs)
  | x == 'M' = run (move rover) xs
  | x == 'L' = run (turnLeft rover) xs
  | x == 'R' = run (turnRight rover) xs

main = do
  [ inputX, inputY, inputD, pattern ] <- sequence [getLine, getLine, getLine, getLine]
  let x = read inputX :: Int
  let y = read inputY :: Int
  let d = read inputD :: Direction
  let rover  = Rover (Position x y) d
  print $ run rover pattern
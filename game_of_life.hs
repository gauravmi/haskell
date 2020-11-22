data Cell = DeadCell { x, y :: Int } | Cell { x, y :: Int } deriving (Show, Eq)

top, bottom, left, right :: Cell -> Cell
top cell = Cell (x cell) (y cell + 1)
bottom cell = Cell (x cell) (y cell - 1)
left cell = Cell (x cell - 1) (y cell)
right cell = Cell (x cell + 1) (y cell)

isAlive :: Cell -> Bool
isAlive cell = cell `elem` aliveCells

neighbors :: Cell -> [Cell]
neighbors cell =
  [top cell, bottom cell, left cell, right cell, left $ top cell, top $ right cell, bottom $ left cell, bottom $ right cell]

aliveNeighbors :: Cell -> Int
aliveNeighbors cell = length $ filter isAlive (neighbors cell)

loneliness, overcrowding, unchanged :: Cell -> Bool
loneliness cell   = aliveNeighbors cell < 2
overcrowding cell = aliveNeighbors cell > 3
unchanged cell    = let length = aliveNeighbors cell in length == 2 || length == 3

aliveCells = [Cell 1 1]

evolve :: Cell -> Cell
evolve cell
  | lonely cell   = DeadCell (x cell) (y cell)
  | overcrowded cell = DeadCell (x cell) (y cell)
  | unchanged cell    = cell
  | otherwise         = cell

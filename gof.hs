neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1), (x+1, y+1), (x-1, y-1)]

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) ys
  | x `elem` ys = x: intersect xs ys
  | otherwise = intersect xs ys

rules :: (Int, Int) -> Bool
rules cell
  | aliveNeighbours  < 2 = False
  | aliveNeighbours  > 3 = False
  | alive && (aliveNeighbours == 3 ||  aliveNeighbours == 2) = True
  | not alive && (aliveNeighbours == 3 ||  aliveNeighbours == 2) = False
  | not alive && aliveNeighbours == 3 = True
  where aliveNeighbours =  length $ intersect (neighbours cell) aliveCells
        alive =  cell `elem` aliveCells

aliveCells :: [(Int, Int)]
aliveCells = [(1,1), (1,2), (2,1), (2,2)]

input :: [(Int, Int)]
input = [(1,1), (1,2), (2,1), (2,2)]
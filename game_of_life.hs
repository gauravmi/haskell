neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1), (x+1, y+1), (x-1, y-1)]

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) ys
  | x `elem` ys = x: intersect xs ys
  | otherwise = intersect xs ys

lonelinessRule :: [(Int, Int)] -> (Int, Int) -> Bool
lonelinessRule aliveCells cell
  | aliveNeighbours  < 2 = False
  | otherwise            = True
  where aliveNeighbours =  length (intersect (neighbours cell) aliveCells)

overcrowdingRule :: [(Int, Int)] -> (Int, Int) -> Bool
overcrowdingRule aliveCells cell
  | aliveNeighbours  > 3 = False
  | otherwise            = True
  where aliveNeighbours =  length $ intersect (neighbours cell) aliveCells

noChangeRule :: [(Int, Int)] -> (Int, Int) -> Bool
noChangeRule aliveCells cell
  | alive && optimumCrowd     = True
  | not alive && optimumCrowd = False
  | otherwise                 = False
  where alive =  cell `elem` aliveCells
        optimumCrowd = aliveNeighbours == 3 ||  aliveNeighbours == 2
        aliveNeighbours =  length $ intersect (neighbours cell) aliveCells

deadToAliveRule :: [(Int, Int)] -> (Int, Int) -> Bool
deadToAliveRule aliveCells cell
  | isDead && aliveNeighbours == 3 = True
  | otherwise                      = False
  where isDead = not (cell `elem`\ aliveCells)
        aliveNeighbours = length $ intersect (neighbours cell) aliveCells

deadToAlive :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
deadToAlive _ [] = []
deadToAlive aliveCells (x:xs) =
  (deadToAlive aliveCells xs) ++ (filter (deadToAliveRule aliveCells) (neighbours x))

rules  = [noChangeRule, overcrowdingRule, lonelinessRule]

blockPattern   :: [(Int, Int)]
boatPattern    :: [(Int, Int)]
blinkerPattern :: [(Int, Int)]
toadPattern    :: [(Int, Int)]

blockPattern   = [(1,1), (1,2), (2,1), (2,2)]
boatPattern    = [(0, 1),(1, 0),(2, 1),(0, 2),(1, 2)]
blinkerPattern = [(1, 1),(1, 0),(1, 2)]
toadPattern    = [(1, 1),(1, 2),(1, 3),(2, 2),(2, 3),(2, 4)]

runBlockPattern (x:xs) = any (==True) [r blockPattern x | r<- rules]
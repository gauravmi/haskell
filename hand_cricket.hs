data Player = Player {score:: Int, isBatsman:: Bool} deriving (Show)
basePlayer = Player 0
baseBatsman = Player 0 True
baseBowler = Player 0 False

data HandGesture = HandGesture {player:: Player, number::Int} deriving (Show)

winner :: Player -> Player -> Maybe Player
winner p1 p2
  | score p1 == score p2 = Nothing
  | score p1 > score p2 = Just p1
  | otherwise = Just p2

updateScore :: HandGesture -> Player
updateScore handGesture = Player ((score (player (handGesture))) + number handGesture) (isBatsman (player (handGesture)))

throw :: HandGesture -> HandGesture -> (Player, Player)
throw handGesture1 handGesture2
  | isOut     = (player handGesture1, player handGesture2)
  | isBatting = (updateScore handGesture1, player handGesture2)
  | otherwise = (player handGesture1, updateScore handGesture2)
  where isOut = number handGesture1 == number handGesture2
        isBatting = isBatsman (player handGesture1)
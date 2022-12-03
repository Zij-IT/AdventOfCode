import Data.Bifunctor (bimap)

data Play = Rock | Paper | Scissors
data Result = Win | Lose | Tie

firstParse :: String -> Play
firstParse s = case s of
  "X" -> Rock
  "Y" -> Paper
  "Z" -> Scissors
  _   -> error "Invalid Input"

secondParse :: String -> Result
secondParse s = case s of
  "X" -> Lose
  "Y" -> Tie
  "Z" -> Win
  _   -> error "Invalid Input"
  
scorePair :: (Play, Play) -> Int
scorePair  p = case p of
  (Rock, Rock) -> 1 + 3
  (Rock, Paper) -> 2 + 6
  (Rock, Scissors) -> 3 + 0
  (Paper, Rock) ->  1 + 0
  (Paper, Paper) -> 2 + 3
  (Paper, Scissors) -> 3 + 6
  (Scissors, Rock) -> 1 + 6
  (Scissors, Paper) -> 2 + 0
  (Scissors, Scissors) -> 3 + 3 
    
partOne :: [(Play, Play)] -> Int
partOne = sum . map scorePair 
        
partTwo :: [(Play, Result)] -> Int
partTwo = sum . map (scorePair . resToPlay)
  where
    -- This part may seem odd, because (Rock, Win) reads like "How do I win with Rock" but it's
    -- actually 'How do I win against Rock'
    resToPlay :: (Play, Result) -> (Play, Play)
    resToPlay p = case p of
      (Rock, Win) -> (Rock, Paper)
      (Rock, Tie) -> (Rock, Rock)
      (Rock, Lose) -> (Rock, Scissors)
      (Paper, Win) -> (Paper, Scissors)
      (Paper, Tie) -> (Paper, Paper)
      (Paper, Lose) -> (Paper, Rock)
      (Scissors, Win) -> (Scissors, Rock)
      (Scissors, Tie) -> (Scissors, Scissors)
      (Scissors, Lose) -> (Scissors, Paper)

clean :: String -> (String -> b) -> [(Play, b)]
clean s toB = map toPair . lines $ s
  where
    toPair line = case line of
      ('A':_:me) -> (Rock, toB me)
      ('B':_:me) -> (Paper, toB me)
      ('C':_:me) -> (Scissors, toB me)
      _          -> error $ "Malformed input: '" ++ line ++ "'"

main :: IO ()
main  = do
  input <- readFile "./input/day02.txt"
  let cleanInput = clean input in do
    print . partOne $ cleanInput firstParse
    print . partTwo $ cleanInput secondParse
    
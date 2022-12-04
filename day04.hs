import Data.List(lines, groupBy, sortBy)
import Data.Bifunctor (bimap)
import qualified Data.Set as S

clean :: String -> [((Int, Int), (Int, Int))]
clean = map readPair . lines
  where
    readPair :: String -> ((Int, Int), (Int, Int))
    readPair = bimap readRange (readRange . tail) . break (== ',')
  
    readRange :: String -> (Int, Int)
    readRange = bimap read (read . tail) . break (=='-')

partOne :: [((Int, Int), (Int, Int))] -> Int
partOne = length . filter fullyOverlaps
  where
    fullyOverlaps :: ((Int, Int), (Int, Int)) -> Bool
    fullyOverlaps ((xb, xe), (yb, ye)) = (xb >= yb && xe <= ye) || (yb >= xb && ye <= xe)

partTwo :: [((Int, Int), (Int, Int))] -> Int
partTwo = length . filter overlaps
  where
    overlaps :: ((Int, Int), (Int, Int)) -> Bool
    overlaps ((xb, xe), (yb, ye)) = ye - yb + xe - xb + 2 > S.size (S.union (S.fromAscList [yb..ye]) (S.fromAscList [xb .. xe]))

main :: IO ()
main = do
  input <- readFile "./input/day04.txt"
  let cleanInput = clean input in do
    print $ partOne cleanInput
    print $ partTwo cleanInput